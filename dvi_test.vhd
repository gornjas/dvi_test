library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity dvi_test is
    generic (
	C_bpp: natural := 8
    );
    port (
	clk, pixclk, pixclk_x5: in std_logic;
	mode: in std_logic_vector(3 downto 0);
	dv_clk, dv_r, dv_g, dv_b: out std_logic_vector(1 downto 0)
    );
end dvi_test;

architecture x of dvi_test is

    type T_modeline is record
	pixclk: natural;
	hdisp: natural;
	hsyncstart: natural;
	hsyncend: natural;
	htotal: natural;
	vdisp: natural;
	vsyncstart: natural;
	vsyncend: natural;
	vtotal: natural;
	hsyncn: natural;
	vsyncn: natural;
	interlace: natural;
    end record T_modeline;

    type T_modelines is array (0 to 7) of T_modeline;
    constant C_ml: T_modelines := (
	( -- 0: 1280x720p @ 60 Hz, 16:9
	    74250, 1280, 1390, 1430, 1650, 720, 725, 730, 750, 0, 0, 0
	),
	( -- 1: 1920x1080i @ 60 Hz, 16:9
	    74250, 1920, 2008, 2052, 2200, 1080, 1084, 1094, 1125, 0, 0, 1
	),
	( -- 2: 1280x720p @ 50 Hz, 16:9
	    74250, 1280, 1720, 1760, 1980, 720, 725, 730, 750, 0, 0, 0
	),
	( -- 3: 1920x1080i @ 50 Hz, 16:9
	    74250, 1920, 2448, 2492, 2640, 1080, 1084, 1094, 1125, 0, 0, 1
	),
	( -- 4: 1920x1080p @ 30 Hz, 16:9
	    74250, 1920, 2008, 2052, 2200, 1080, 1084, 1089, 1125, 0, 0, 0
	),
	( -- 5: 1920x1080p @ 25 Hz, 16:9
	    74250, 1920, 2448, 2492, 2640, 1080, 1084, 1089, 1125, 0, 0, 0
	),
	( -- 6: 1920x1080p @ 24 Hz, 16:9
	    74250, 1920, 2558, 2602, 2750, 1080, 1084, 1089, 1125, 0, 0, 0
	),
	( -- 7: 1280x720p @ 30 Hz, 16:9
	    74250, 1280, 3040, 3080, 3300, 720, 725, 730, 750, 0, 0, 0
	)
    );

    type T_fifo is array (0 to 2047) of
      std_logic_vector(C_bpp * 3 - 1 downto 0);
    signal M_fifo: T_fifo; -- WR in clk, RD in pixclk clock domain
    attribute syn_ramstyle: string; -- Lattice Diamond
    attribute syn_ramstyle of M_fifo: signal is "no_rw_check";

    -- pixclk domain, registers
    signal R_fifo_tail: std_logic_vector(10 downto 0);
    signal R_r_mem, R_g_mem, R_b_mem: std_logic_vector(C_bpp - 1 downto 0);
    signal R_r, R_g, R_b: std_logic_vector(7 downto 0);
    signal R_hsync, R_vsync, R_blank: std_logic;

    -- pixclk domain, wires
    signal dv_vsync, dv_hsync, dv_frame, dv_active: std_logic;
    signal dv_frame_done: std_logic;

    -- pixclk -> clk clock domain crossing synchronizers
    signal R_t_fifo_sync: std_logic_vector(2 downto 0);
    signal R_t_frame_sync: std_logic_vector(2 downto 0);
    signal R_t_frame_done_sync: std_logic_vector(2 downto 0);

    -- main clk domain, fifo clk -> pixclk clock domain
    signal R_fifo_tail_cdc: std_logic_vector(10 downto 4);
    signal R_fifo_head: std_logic_vector(10 downto 0);

    -- main clk domain, test picture generator
    signal R_t_hpos, R_t_vpos: std_logic_vector(11 downto 0);
    signal R_t_framecnt: std_logic_vector(9 downto 0);
    signal R_t_active: boolean;

    -- clk domain, (mostly) static linemode configuration data
    signal R_mode: natural;
    signal R_hdisp: std_logic_vector(11 downto 0);
    signal R_hsyncstart: std_logic_vector(11 downto 0);
    signal R_hsyncend: std_logic_vector(11 downto 0);
    signal R_htotal: std_logic_vector(11 downto 0);
    signal R_vdisp: std_logic_vector(10 downto 0);
    signal R_vsyncstart: std_logic_vector(10 downto 0);
    signal R_vsyncend: std_logic_vector(10 downto 0);
    signal R_vtotal: std_logic_vector(10 downto 0);
    signal R_interlace: std_logic;

begin
    -- Test picture generator
    process(clk)
	variable fifo_head_next: std_logic_vector(10 downto 0);
	variable tsum1, tsum2: std_logic_vector(11 downto 0);
	variable r, g, b: std_logic_vector(7 downto 0);
	variable wa: natural;
    begin
	if rising_edge(clk) then
	    -- configuration
	    R_mode <= conv_integer(mode);
	    R_hdisp <= conv_std_logic_vector(C_ml(R_mode).hdisp, 12);
	    R_hsyncstart <= conv_std_logic_vector(C_ml(R_mode).hsyncstart, 12);
	    R_hsyncend <= conv_std_logic_vector(C_ml(R_mode).hsyncend, 12);
	    R_htotal <= conv_std_logic_vector(C_ml(R_mode).htotal, 12);
	    R_vdisp <= conv_std_logic_vector(C_ml(R_mode).vdisp, 11);
	    R_vsyncstart <= conv_std_logic_vector(C_ml(R_mode).vsyncstart, 11);
	    R_vsyncend <= conv_std_logic_vector(C_ml(R_mode).vsyncend, 11);
	    R_vtotal <= conv_std_logic_vector(C_ml(R_mode).vtotal, 11);
	    R_interlace <= conv_std_logic_vector(C_ml(R_mode).interlace, 1)(0);

	    -- clock-domain crossing synchronizers (from pixclk)
	    R_t_fifo_sync <= R_fifo_tail(4) & R_t_fifo_sync(2 downto 1);
	    R_t_frame_sync <= dv_frame & R_t_frame_sync(2 downto 1);
	    R_t_frame_done_sync <=
	      dv_frame_done & R_t_frame_done_sync(2 downto 1);

	    if R_t_fifo_sync(1) /= R_t_fifo_sync(0) then
		R_fifo_tail_cdc <= R_fifo_tail(10 downto 4);
	    end if;

	    fifo_head_next := R_fifo_head + 1;
	    if R_t_frame_done_sync(1 downto 0) = "10" then
		R_fifo_head <= (others => '0');
		R_t_hpos <= (others => '0');
		R_t_vpos <= (others => '0');
		R_t_framecnt <= R_t_framecnt + 1;
	    elsif R_t_frame_done_sync(0) = '0' and
	      R_fifo_tail_cdc /= fifo_head_next(10 downto 4) then
		R_fifo_head <= fifo_head_next;
		R_t_hpos <= R_t_hpos + 1;
		if R_t_hpos + 1 = R_hdisp then
		    R_t_hpos <= (others => '0');
		    R_t_vpos <= R_t_vpos + 1;
		    if R_interlace = '1' then
			R_t_vpos <= R_t_vpos + 2;
		    end if;
		end if;
	    end if;

	    if R_t_frame_sync(1 downto 0) = "01" then
		R_t_hpos <= conv_std_logic_vector(2, 12);
	    end if;

	    tsum1 := R_t_hpos + R_t_vpos + R_t_framecnt;
	    tsum2 := R_t_hpos - R_t_vpos + 1;

	    r := R_t_hpos(7 downto 0);
	    g := R_t_vpos(7 downto 0);
	    b := tsum1(9 downto 2);

	    if tsum2(7 downto 1) = 0 then
		if R_t_framecnt(6) = '0' then
		    r := R_t_framecnt(5 downto 0) & "00";
		    g := R_t_framecnt(5 downto 0) & "00";
		    b := R_t_framecnt(5 downto 0) & "00";
		else
		    r := x"fc" xor (R_t_framecnt(5 downto 0) & "00");
		    g := x"fc" xor (R_t_framecnt(5 downto 0) & "00");
		    b := x"fc" xor (R_t_framecnt(5 downto 0) & "00");
		end if;
		if R_t_framecnt(9) = '0' then
		    r := x"00";
		end if;
		if R_t_framecnt(8) = '0' then
		    g := x"00";
		end if;
		if R_t_framecnt(7) = '0' then
		    b := x"00";
		end if;
	    end if;

	    wa := conv_integer(R_fifo_head);
	    M_fifo(wa)(C_bpp * 3 - 1 downto C_bpp * 2) <= r(7 downto 8 - C_bpp);
	    M_fifo(wa)(C_bpp * 2 - 1 downto C_bpp) <= g(7 downto 8 - C_bpp);
	    M_fifo(wa)(C_bpp - 1 downto 0) <= b(7 downto 8 - C_bpp);
	end if;
    end process;

    I_syncgen: entity work.dv_syncgen
    port map (
	pixclk => pixclk,
	-- mode config
	hdisp => R_hdisp,
	hsyncstart => R_hsyncstart,
	hsyncend => R_hsyncend,
	htotal => R_htotal,
	vdisp => R_vdisp,
	vsyncstart => R_vsyncstart,
	vsyncend => R_vsyncend,
	vtotal => R_vtotal,
	interlace => R_interlace,
	-- outputs
	hsync => dv_hsync,
	vsync => dv_vsync,
	active => dv_active,
	frame => dv_frame,
	frame_done => dv_frame_done
    );

    process(pixclk)
	variable ra: natural;
    begin
	if rising_edge(pixclk) then
	    -- from line buffer and dv_syncgen to vga2dvid
	    R_blank <= not dv_active;
	    R_hsync <= dv_hsync;
	    R_vsync <= dv_vsync;
	    if dv_frame_done = '1' then
		R_fifo_tail <= (others => '0');
	    elsif dv_active = '1' then
		R_fifo_tail <= R_fifo_tail + 1;
	    end if;
	    ra := conv_integer(R_fifo_tail);
	    R_r_mem <= M_fifo(ra)(C_bpp * 3 - 1 downto C_bpp * 2);
	    R_g_mem <= M_fifo(ra)(C_bpp * 2 - 1 downto C_bpp);
	    R_b_mem <= M_fifo(ra)(C_bpp - 1 downto 0);
	    R_r(7 downto 8 - C_bpp) <= R_r_mem;
	    R_g(7 downto 8 - C_bpp) <= R_g_mem;
	    R_b(7 downto 8 - C_bpp) <= R_b_mem;
	    case C_bpp is
	    when 7 | 6 | 5 | 4 =>
		R_r(7 - C_bpp downto 0) <=
		  R_r_mem(C_bpp - 1 downto C_bpp * 2 - 8);
		R_g(7 - C_bpp downto 0) <=
		  R_g_mem(C_bpp - 1 downto C_bpp * 2 - 8);
		R_b(7 - C_bpp downto 0) <=
		  R_b_mem(C_bpp - 1 downto C_bpp * 2 - 8);
	    when 3 =>
		R_r(4 downto 0) <= R_r_mem & R_r_mem(2 downto 1);
		R_g(4 downto 0) <= R_g_mem & R_g_mem(2 downto 1);
		R_b(4 downto 0) <= R_b_mem & R_b_mem(2 downto 1);
	    when 2 =>
		R_r(3 downto 0) <= R_r_mem(1 downto 0) & R_r_mem(1 downto 0);
		R_g(3 downto 0) <= R_g_mem(1 downto 0) & R_g_mem(1 downto 0);
		R_b(3 downto 0) <= R_b_mem(1 downto 0) & R_b_mem(1 downto 0);
	    when 1 =>
		R_r <= (others => R_r_mem(0));
		R_g <= (others => R_g_mem(0));
		R_b <= (others => R_b_mem(0));
	    when others =>
	    end case;
	end if;
    end process;

    I_dvid: entity work.vga2dvid
    generic map (
	C_parallel => false,
	C_ddr => true
    )
    port map (
	clk_pixel => pixclk,
	clk_shift => pixclk_x5,
	in_red => R_r,
	in_green => R_g,
	in_blue => R_b,
	in_hsync => R_hsync,
	in_vsync => R_vsync,
	in_blank => R_blank,
	out_clock => dv_clk,
	out_red => dv_r,
	out_green => dv_g,
	out_blue=> dv_b
    );
end x;
