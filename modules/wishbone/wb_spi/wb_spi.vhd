
library ieee;
use ieee.std_logic_1164.all;
use ieee.NUMERIC_STD.ALL;

use work.wishbone_pkg.all;

entity wb_spi is
  generic (
    g_interface_mode      : t_wishbone_interface_mode      := CLASSIC;
    g_address_granularity : t_wishbone_address_granularity := WORD;
    g_divider_len         : integer := 16;
    g_max_char_len        : integer := 128;
    g_num_slaves          : integer := 8;
    g_enable_sdio         : boolean := false
    );
  port(
    clk_sys_i : in std_logic;
    rst_n_i   : in std_logic;

    wb_adr_i   : in  std_logic_vector(4 downto 0);
    wb_dat_i   : in  std_logic_vector(31 downto 0);
    wb_dat_o   : out std_logic_vector(31 downto 0);
    wb_sel_i   : in  std_logic_vector(3 downto 0);
    wb_stb_i   : in  std_logic;
    wb_cyc_i   : in  std_logic;
    wb_we_i    : in  std_logic;
    wb_ack_o   : out std_logic;
    wb_err_o   : out std_logic;
    wb_int_o   : out std_logic;
    wb_stall_o : out std_logic;

    pad_cs_o   : out std_logic_vector(g_num_slaves-1 downto 0);
    pad_sclk_o : out std_logic;
    pad_mosi_o : out std_logic;
    pad_mosi_t : out std_logic;
    pad_mosi_i : in  std_logic := '0';
    pad_miso_i : in  std_logic
    );

end wb_spi;

architecture rtl of wb_spi is
  constant c_slave_interface_mode : t_wishbone_interface_mode := PIPELINED;

  component spi_clgen 
    generic (
      SPI_DIVIDER_LEN : natural := 8
    );
    port (
      clk_in : in std_logic;
      rst    : in std_logic;
      
      enable   : in  std_logic;
      go       : in  std_logic;
      last_clk : in  std_logic;
      divider  : in  std_logic_vector(SPI_DIVIDER_LEN - 1 downto 0);     
      clk_out  : out std_logic;
      pos_edge : out std_logic;
      neg_edge : out std_logic
    );
  end component;

  component spi_shift
    generic (
      SPI_MAX_CHAR : natural := 128;
      SPI_CHAR_LEN_BITS : natural := 7
    );
    port (
      clk : in std_logic;
      rst : in std_logic;
    
      latch      : in  std_logic_vector(3 downto 0);
      byte_sel   : in  std_logic_vector(3 downto 0);
      len        : in  std_logic_vector(SPI_CHAR_LEN_BITS - 1 downto 0);
      lsb        : in  std_logic;
      go         : in  std_logic;
      pos_edge   : in  std_logic;
      neg_edge   : in  std_logic;
      rx_negedge : in  std_logic;
      tx_negedge : in  std_logic;    
      tip        : out std_logic;
      last       : out std_logic;
      p_in       : in  std_logic_vector(31 downto 0);
      p_out      : out std_logic_vector(SPI_MAX_CHAR - 1 downto 0);
      s_clk      : in  std_logic;
      s_in       : in  std_logic;
      s_out      : out std_logic
    );
  end component;

  signal rst : std_logic;

  signal resized_addr : std_logic_vector(c_wishbone_address_width-1 downto 0);
   
  signal wb_m2s : t_wishbone_slave_in;  -- master to slave signals
  signal wb_s2m : t_wishbone_slave_out; -- slave to master signals

  
  constant c_idx_SPI_RX_0    : natural := 0;
  constant c_idx_SPI_RX_1    : natural := 1;
  constant c_idx_SPI_RX_2    : natural := 2;
  constant c_idx_SPI_RX_3    : natural := 3;
  
  constant c_idx_SPI_TX_0    : natural := 0;
  constant c_idx_SPI_TX_1    : natural := 1;
  constant c_idx_SPI_TX_2    : natural := 2;
  constant c_idx_SPI_TX_3    : natural := 3;
  
  constant c_idx_SPI_CTRL    : natural := 4;
  constant c_idx_SPI_DEVIDE  : natural := 5;
  constant c_idx_SPI_SS      : natural := 6;
  
  constant c_zeros : std_logic_vector(31 downto 0) := (others => '0');
  
  -- registers
  signal r_rx      : std_logic_vector(127 downto 0);               -- Rx register
  signal r_ctrl    : std_logic_vector(31 downto 0);                -- Control register
  signal r_divider : std_logic_vector(g_divider_len - 1 downto 0); -- Divider register
  signal r_ss      : std_logic_vector(g_num_slaves - 1 downto 0);  -- Slave select register
  
  signal s_tx_sel: std_logic_vector(3 downto 0);

  -- Control register signals
  signal s_char_len   : std_logic_vector(6 downto 0);
  signal s_go         : std_logic;
  signal s_rx_negedge : std_logic;
  signal s_tx_negedge : std_logic;
  signal s_lsb        : std_logic;
  signal s_ie         : std_logic;
  signal s_ass        : std_logic;
  signal s_sdio_en    : std_logic;
  signal s_sdio_bits  : std_logic_vector(6 downto 0);
  -- Control register read signal
  signal s_ctrl    : std_logic_vector(31 downto 0);

  
  signal s_tip : std_logic;
  signal s_last_bit : std_logic;
  signal s_pos_edge : std_logic;
  signal s_neg_edge: std_logic;
  
  signal s_pad_sclk: std_logic;
  signal s_pad_miso: std_logic;
  signal s_pad_mosi: std_logic;
begin
  
  resized_addr(4 downto 0)                          <= wb_adr_i;
  resized_addr(c_wishbone_address_width-1 downto 5) <= (others => '0');

  
  U_Adapter : wb_slave_adapter
    generic map (
      g_master_use_struct  => true,
      g_master_mode        => c_slave_interface_mode,
      g_master_granularity => WORD,
      g_slave_use_struct   => false,
      g_slave_mode         => g_interface_mode,
      g_slave_granularity  => g_address_granularity)
    port map (
      clk_sys_i  => clk_sys_i,
      rst_n_i    => rst_n_i,
      master_i   => wb_s2m,
      master_o   => wb_m2s,
      sl_adr_i   => resized_addr,
      sl_dat_i   => wb_dat_i,
      sl_sel_i   => wb_sel_i,
      sl_cyc_i   => wb_cyc_i,
      sl_stb_i   => wb_stb_i,
      sl_we_i    => wb_we_i,
      sl_dat_o   => wb_dat_o,
      sl_ack_o   => wb_ack_o,
      sl_stall_o => wb_stall_o,
      sl_int_o   => wb_int_o,
      sl_err_o   => wb_err_o);


  
  p_data: process(clk_sys_i, rst_n_i)
  begin
    if rst_n_i = '0' then
      wb_s2m.dat <= (others => '0');   
    elsif rising_edge(clk_sys_i) then
      if wb_m2s.cyc  = '1' and wb_m2s.stb = '1' and wb_m2s.we = '0' then
        if wb_m2s.adr(2 downto 0) = "000" then
          wb_s2m.dat <= r_rx(31 downto 0);
        elsif wb_m2s.adr(2 downto 0) = "001" and g_max_char_len > 32 then
          wb_s2m.dat <= r_rx(63 downto 32);
        elsif wb_m2s.adr(2 downto 0) = "010" and g_max_char_len > 64  then
          wb_s2m.dat <= r_rx(95 downto 64);
        elsif wb_m2s.adr(2 downto 0) = "011" and g_max_char_len > 96  then
          wb_s2m.dat <= r_rx(127 downto 96);
        elsif wb_m2s.adr(2 downto 0) = "100" then
          wb_s2m.dat <= s_ctrl;
        elsif wb_m2s.adr(2 downto 0) = "101" then
          wb_s2m.dat <= c_zeros(31 downto r_divider'length) & r_divider;
        elsif wb_m2s.adr(2 downto 0) = "110" then
          wb_s2m.dat <= c_zeros(31 downto r_ss'length) & r_ss;
        else
          wb_s2m.dat <= (others => '0');
        end if;
      end if;
    end if;
  end process;
  
  
  p_ctrl: process(clk_sys_i, rst_n_i)
  begin
    if rst_n_i = '0' then
      r_ctrl <= (others => '0');   
    elsif rising_edge(clk_sys_i) then
      if s_tip = '1' then
        if s_last_bit = '1' and s_pos_edge = '1' then
          r_ctrl(8) <= '0';        
        end if;
      else        
        if wb_m2s.cyc  = '1' and wb_m2s.stb = '1' and wb_m2s.we = '1' and wb_m2s.adr(2 downto 0) = "100" then
          for i in 0 to r_ctrl'length - 1 loop
            if wb_m2s.sel(i/8) = '1' then
              r_ctrl(i) <= wb_m2s.dat(i);
            end if;
          end loop;
        end if;
      end if;
    end if;
  end process;

  s_char_len   <= r_ctrl( 6 downto 0);
  s_go         <= r_ctrl( 8);
  s_rx_negedge <= r_ctrl( 9);
  s_tx_negedge <= r_ctrl(10);
  s_lsb        <= r_ctrl(11);
  s_ie         <= r_ctrl(12);
  s_ass        <= r_ctrl(13);
  s_ctrl <= c_zeros(31 downto 24) &
            s_sdio_en &
            s_sdio_bits & 
            c_zeros(15 downto 14) & r_ctrl(13 downto 8) &
            '0' & r_ctrl(6 downto 0);


  
  p_divider: process(clk_sys_i, rst_n_i)
  begin
    if rst_n_i = '0' then
      r_divider <= (others => '0');   
    elsif rising_edge(clk_sys_i) then
      if wb_m2s.cyc  = '1' and wb_m2s.stb = '1' and wb_m2s.we = '1' and wb_m2s.adr(2 downto 0) = "101" then
        for i in 0 to r_divider'length - 1 loop
          if wb_m2s.sel(i/8) = '1' then
            r_divider(i) <= wb_m2s.dat(i);
          end if;
        end loop;
      end if;
    end if;
  end process;
  
  p_ss: process(clk_sys_i, rst_n_i)
  begin
    if rst_n_i = '0' then
      r_ss <= (others => '0');   
    elsif rising_edge(clk_sys_i) then
      if wb_m2s.cyc  = '1' and wb_m2s.stb = '1' and wb_m2s.we = '1' and wb_m2s.adr(2 downto 0) = "110" then
        for i in 0 to r_ss'length - 1 loop -- implement wishbone byte select
          if wb_m2s.sel(i/8) = '1' then
            r_ss(i) <= wb_m2s.dat(i);
          end if;
        end loop;
      end if;
    end if;  
  end process;
  
  p_ack: process(clk_sys_i, rst_n_i)
  begin
    if(rst_n_i = '0') then
      wb_s2m.ack <= '0';
    elsif rising_edge(clk_sys_i) then
      if(wb_s2m.ack = '1' and c_slave_interface_mode = CLASSIC) then
        wb_s2m.ack <= '0';
      else
        wb_s2m.ack <= wb_m2s.cyc and wb_m2s.stb;
      end if;
    end if;
  end process;	

  wb_s2m.stall <= not rst_n_i; -- we do not support cycles while reset
  wb_s2m.err <= '0';
  wb_s2m.rty <= '0';

  
  rst <= not rst_n_i;
  
  p_interrupt: process(clk_sys_i, rst_n_i)
  begin
    if(rst_n_i = '0') then
      wb_s2m.int <= '0';
    elsif rising_edge(clk_sys_i) then
      if s_ie = '1' and s_tip = '1'  and  s_last_bit = '1' and s_pos_edge = '1' then
        wb_s2m.int <= '1';
      elsif (wb_s2m.ack = '1') then
        wb_s2m.int <= '0';
      end if;
    end if;
  end process;

  U_clgen: spi_clgen
    generic map (
      SPI_DIVIDER_LEN => g_divider_len
    )
    port map (
      clk_in =>  clk_sys_i,
      rst => rst,
      go => s_go,
      enable => s_tip,
      last_clk => s_last_bit,
      divider => r_divider,
      clk_out => s_pad_sclk,
      pos_edge => s_pos_edge, 
      neg_edge => s_neg_edge
    );
  pad_sclk_o <= s_pad_sclk;

  s_tx_sel(0) <= '1' when wb_m2s.cyc  = '1' and wb_m2s.stb = '1' and wb_m2s.we = '1' and wb_m2s.adr(2 downto 0) = "000"  else '0';
  s_tx_sel(1) <= '1' when wb_m2s.cyc  = '1' and wb_m2s.stb = '1' and wb_m2s.we = '1' and wb_m2s.adr(2 downto 0) = "001"  else '0';
  s_tx_sel(2) <= '1' when wb_m2s.cyc  = '1' and wb_m2s.stb = '1' and wb_m2s.we = '1' and wb_m2s.adr(2 downto 0) = "010"  else '0';
  s_tx_sel(3) <= '1' when wb_m2s.cyc  = '1' and wb_m2s.stb = '1' and wb_m2s.we = '1' and wb_m2s.adr(2 downto 0) = "011"  else '0';  
  
  U_shift: spi_shift
  generic map(
    SPI_MAX_CHAR => g_max_char_len,
    SPI_CHAR_LEN_BITS => f_ceil_log2(g_max_char_len)
  )
  port map (
    clk => clk_sys_i,
    rst => rst,
    latch => s_tx_sel,
    byte_sel => wb_m2s.sel,
    len      => s_char_len,
    lsb      => s_lsb,
    go       => s_go,
    pos_edge => s_pos_edge,
    neg_edge => s_neg_edge,
    rx_negedge => s_rx_negedge,
    tx_negedge => s_tx_negedge,
    
    tip    => s_tip,
    last   => s_last_bit,
    p_in   => wb_m2s.dat,
    p_out  => r_rx,
    
    s_clk  => s_pad_sclk,
    s_in   => s_pad_miso,
    s_out  => s_pad_mosi
   );


GEN_SDIO: if g_enable_sdio = true generate
   signal r_sdio_bits_cnt : std_logic_vector(6 downto 0);
   signal r_sdio_en       : std_logic;
begin

  s_sdio_bits  <= r_ctrl(22 downto 16);
  s_sdio_en    <= r_ctrl(23);
  s_pad_miso <= pad_mosi_i when r_sdio_en = '1' else pad_miso_i;
  pad_mosi_t <= '1' when r_sdio_en = '1' and r_sdio_bits_cnt = "0000000" else '0';
  
  p_sdio_bits: process(clk_sys_i, rst_n_i)
  begin
    if rst_n_i = '0' then      
      r_sdio_bits_cnt <= (others => '0');
      r_sdio_en <= '0';
    elsif rising_edge(clk_sys_i) then
      if s_go = '1' and s_tip = '0' then
         r_sdio_en       <= s_sdio_en;
         r_sdio_bits_cnt <= s_sdio_bits;
      elsif unsigned(r_sdio_bits_cnt) /= 0 and s_neg_edge = '1' then
         r_sdio_bits_cnt <= std_logic_vector(unsigned(r_sdio_bits_cnt) - 1);
      end if;
    end if;        
  end process;
  
end generate;

GEN_NO_SDIO: if g_enable_sdio = false generate

  s_sdio_bits  <= (others => '0');
  s_sdio_en <= '0';
  s_pad_miso <= pad_miso_i;
  pad_mosi_t <= '0';
  
end generate;

pad_mosi_o <= s_pad_mosi;
pad_cs_o <= not (r_ss) when  (s_tip = '1' and s_ass = '1') or (s_ass = '0') else (others => '1');
    
 
end rtl;
