library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity dv_syncgen is
    port (
	pixclk: in std_logic;
	-- inputs: video mode configuration
	hdisp: in std_logic_vector(11 downto 0);
	hsyncstart: in std_logic_vector(11 downto 0);
	hsyncend: in std_logic_vector(11 downto 0);
	htotal: in std_logic_vector(11 downto 0);
	vdisp: in std_logic_vector(10 downto 0);
	vsyncstart: in std_logic_vector(10 downto 0);
	vsyncend: in std_logic_vector(10 downto 0);
	vtotal: in std_logic_vector(10 downto 0);
	interlace: in std_logic;
	-- outputs: sync signals
	hpos: out std_logic_vector(10 downto 0);
	hsync: out std_logic;
	vsync: out std_logic;
	frame: out std_logic;
	active: out std_logic
    );
end dv_syncgen;

architecture x of dv_syncgen is
    signal R_hstate: std_logic_vector(1 downto 0) := (others => '0');
    signal R_vstate: std_logic_vector(1 downto 0) := (others => '0');
    signal R_hpos: std_logic_vector(11 downto 0) := (others => '0');
    signal R_vpos: std_logic_vector(10 downto 0) := (others => '0');
    signal R_hbound: std_logic_vector(47 downto 0) := (others => '0');
    signal R_vbound: std_logic_vector(43 downto 0) := (others => '0');
    signal R_vsync_delay: std_logic_vector(11 downto 0) := (others => '0');
    signal R_skip_line: boolean := false;
    signal R_hsync: std_logic := '0';
    signal R_vsync: std_logic := '0';
    signal R_active: std_logic := '0';
    signal R_frame: std_logic := '0';

begin
    process(pixclk)
	variable hsync: boolean;
    begin
	if rising_edge(pixclk) then
	    R_hpos <= R_hpos + 1;
	    hsync := false;
	    if R_hpos = R_hbound(11 downto 0) then
		R_hstate <= R_hstate + 1;
		if R_hstate = "11" then
		    R_hbound <= htotal & hsyncend & hsyncstart & hdisp;
		    R_hpos <= conv_std_logic_vector(1, 12);
		else
		    R_hbound(35 downto 0) <= R_hbound(47 downto 12);
		end if;
		case R_hstate is
		when "00" =>
		    R_active <= '0';
		when "01" =>
		    R_hsync <= '1';
		    hsync := true;
		when "10" =>
		    R_hsync <= '0';
		when others => -- "11"
		    if R_vstate = "00" and not R_skip_line then
			R_active <= '1';
		    end if;
		    R_skip_line <= false;
		end case;
	    end if;
	    if hsync then
		R_vpos <= R_vpos + 1;
		if interlace = '1' then
		    R_vpos <= R_vpos + 2;
		end if;
		if R_vpos(10 downto 1) = R_vbound(10 downto 1) and
		  (interlace = '1' or (R_vpos(0) = R_vbound(0))) then
		    R_vstate <= R_vstate + 1;
		    R_vbound(32 downto 0) <= R_vbound(43 downto 11);
		    case R_vstate is
		    when "00" =>
			R_frame <= interlace and not R_frame;
		    when "01" =>
			if R_frame = '0' then
			    R_vsync <= '1';
			else
			    R_vsync_delay <= '0' & htotal(11 downto 1);
			end if;
		    when "10" =>
			if R_frame = '0' then
			    R_vsync <= '0';
			else
			    R_vsync_delay <= '0' & htotal(11 downto 1);
			end if;
		    when "11" =>
			R_vbound <= vtotal & vsyncend & vsyncstart & vdisp;
			R_vpos <= conv_std_logic_vector(1, 11);
			if interlace = '1' then
			    if R_frame = '0' then
				R_vpos <= conv_std_logic_vector(2, 11);
			    elsif vtotal(0) = '1' then
				R_skip_line <= true;
			    end if;
			end if;
		    when others =>
			-- nothing to do, appease the tools
		    end case;
		end if;
	    end if;
	    if R_vsync_delay(11) = '0' then
		R_vsync_delay <= R_vsync_delay - 1;
		if R_vsync_delay = 0 then
		    R_vsync <= not R_vsync;
		end if;
	    end if;
	end if;
    end process;

    hpos <= R_hpos(10 downto 0) when R_active = '1' else (others => '0');
    hsync <= R_hsync;
    vsync <= R_vsync;
    frame <= R_frame;
    active <= R_active;
end x;
