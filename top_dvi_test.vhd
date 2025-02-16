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

    signal R_mode: std_logic_vector(3 downto 0);
    signal R_key_sync, R_key_last: std_logic_vector(1 downto 0);
    signal R_debounce_cnt: integer;

begin
    I_pll: entity work.pll_25m
    port map(
	clk_25m => clk_25m,
--	clk_123m75 => clk,
	clk_135m => clk,
	clk_74m25 => pixclk,
	clk_371m25 => pixclk_x5
    );

    I_test: entity work.dvi_test
    port map (
	clk => clk,
	pixclk => pixclk,
	pixclk_x5 => pixclk_x5,
	mode => R_mode,
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

    process(pixclk)
    begin
	if rising_edge(pixclk) then
	    -- mode select
	    R_key_sync <= btn_up & btn_down;
	    if R_key_last /= R_key_sync then
		R_debounce_cnt <= R_debounce_cnt - 1;
		if R_debounce_cnt < 0 then
		    R_key_last <= R_key_sync;
		    if R_key_sync(1) = '1' then
			R_mode(2 downto 0) <= R_mode(2 downto 0) + 1;
		    end if;
		    if R_key_sync(0) = '1' then
			R_mode(2 downto 0) <= R_mode(2 downto 0) - 1;
		    end if;
		end if;
	    else
		R_debounce_cnt <= 1500000;
	    end if;
	end if;
    end process;

    led(3 downto 0) <= R_mode;
end x;
