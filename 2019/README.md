https://adventofcode.com/2019/

	synack@katana ~/src/advent2019(master) $ gprbuild 
	using project file advent_2019.gpr
	Setup
	   [mkdir]        object directory for project Advent_2019
	Compile
	   [Ada]          run.adb
	   [Ada]          advent1.adb
	   [Ada]          advent3.adb
	   [Ada]          advent4.adb
	   [Ada]          advent7.adb
	   [Ada]          intcode.adb
	   [Ada]          stack.adb
	Bind
	   [gprbind]      run.bexch
	   [Ada]          run.ali
	   [gprbind]      advent1.bexch
	   [Ada]          advent1.ali
	   [gprbind]      advent3.bexch
	   [Ada]          advent3.ali
	   [gprbind]      advent4.bexch
	   [Ada]          advent4.ali
	   [gprbind]      advent7.bexch
	   [Ada]          advent7.ali
	Link
	   [link]         run.adb
	   [link]         advent1.adb
	   [link]         advent3.adb
	   [link]         advent4.adb
	   [link]         advent7.adb
	synack@katana ~/src/advent2019(master) $ obj/run input/advent9-boost 
	> 2
	 XXXXXXXXX (don't cheat!)
	HALT
	--------------------------
	Memory Used:  1076
	Cycle count:  371205
