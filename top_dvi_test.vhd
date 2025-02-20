library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

library ecp5u;
use ecp5u.components.all;

entity top_dvi_test is
    port (
	clk_25m: in std_logic;
	gpdi_dp: out std_logic_vector(3 downto 0);
	btn_up, btn_down, btn_left, btn_right: in std_logic;
	led: out std_logic_vector(7 downto 0)
    );
end;

architecture x of top_dvi_test is
    signal clk, pixclk, pixclk_x5: std_logic;

    signal dv_crgb: std_logic_vector(7 downto 0);
    signal ddr_d: std_logic_vector(3 downto 0);

    signal R_cnt: std_logic_vector(32 downto 0);
    signal mode: std_logic_vector(3 downto 0);

begin
    R_cnt <= (others => '0') when btn_up = '1'
      else R_cnt + 1 when rising_edge(clk) and btn_down = '0';
    mode <= x"1" when btn_left = '1'
      else x"3" when btn_right = '1'
      else '0' & R_cnt(32 downto 30);
    led(3 downto 0) <= mode;

    I_pll: entity work.pll_25m
    port map(
	clk_25m => clk_25m,
	clk_123m75 => clk,
	clk_74m25 => pixclk,
	clk_371m25 => pixclk_x5
    );

    I_test: entity work.dvi_test
    port map (
	clk => clk,
	pixclk => pixclk,
	pixclk_x5 => pixclk_x5,
	mode => mode,
	dv_clk => dv_crgb(7 downto 6),
	dv_r => dv_crgb(5 downto 4),
	dv_g => dv_crgb(3 downto 2),
	dv_b => dv_crgb(1 downto 0)
    );

    G_oddr: for i in 0 to 3 generate
    I_oddr: oddrx1f port map (
	sclk => pixclk_x5,
	rst => '0',
	d0 => dv_crgb(2 * i),
	d1 => dv_crgb(2 * i + 1),
	q => gpdi_dp(i)
    );
    end generate;
end x;
