
O
*Debug cores have already been implemented
153*	chipscopeZ16-240h px� 
Q
Command: %s
53*	vivadotcl2 
place_design2default:defaultZ4-113h px� 
�
@Attempting to get a license for feature '%s' and/or device '%s'
308*common2"
Implementation2default:default2
xczu3cg2default:defaultZ17-347h px� 
�
0Got license for feature '%s' and/or device '%s'
310*common2"
Implementation2default:default2
xczu3cg2default:defaultZ17-349h px� 
P
Running DRC with %s threads
24*drc2
62default:defaultZ23-27h px� 
V
DRC finished with %s
79*	vivadotcl2
0 Errors2default:defaultZ4-198h px� 
e
BPlease refer to the DRC report (report_drc) for more information.
80*	vivadotclZ4-199h px� 
p
,Running DRC as a precondition to command %s
22*	vivadotcl2 
place_design2default:defaultZ4-22h px� 
P
Running DRC with %s threads
24*drc2
62default:defaultZ23-27h px� 
V
DRC finished with %s
79*	vivadotcl2
0 Errors2default:defaultZ4-198h px� 
e
BPlease refer to the DRC report (report_drc) for more information.
80*	vivadotclZ4-199h px� 
U

Starting %s Task
103*constraints2
Placer2default:defaultZ18-103h px� 
}
BMultithreading enabled for place_design using a maximum of %s CPUs12*	placeflow2
62default:defaultZ30-611h px� 
v

Phase %s%s
101*constraints2
1 2default:default2)
Placer Initialization2default:defaultZ18-101h px� 
�

Phase %s%s
101*constraints2
1.1 2default:default29
%Placer Initialization Netlist Sorting2default:defaultZ18-101h px� 
�
r%sTime (s): cpu = %s ; elapsed = %s . Memory (MB): peak = %s ; gain = %s ; free physical = %s ; free virtual = %s
480*common2.
Netlist sorting complete. 2default:default2
00:00:00.022default:default2
00:00:00.022default:default2
4974.3162default:default2
0.0002default:default2
28422default:default2
73222default:defaultZ17-722h px� 
Z
EPhase 1.1 Placer Initialization Netlist Sorting | Checksum: adf0f9b7
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:00:00.05 ; elapsed = 00:00:00.12 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2842 ; free virtual = 73222default:defaulth px� 
�
r%sTime (s): cpu = %s ; elapsed = %s . Memory (MB): peak = %s ; gain = %s ; free physical = %s ; free virtual = %s
480*common2.
Netlist sorting complete. 2default:default2
00:00:00.022default:default2
00:00:00.012default:default2
4974.3162default:default2
0.0002default:default2
28352default:default2
73152default:defaultZ17-722h px� 
�

Phase %s%s
101*constraints2
1.2 2default:default2F
2IO Placement/ Clock Placement/ Build Placer Device2default:defaultZ18-101h px� 
E
%Done setting XDC timing constraints.
35*timingZ38-35h px� 
g
RPhase 1.2 IO Placement/ Clock Placement/ Build Placer Device | Checksum: bffd38a9
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:00:36 ; elapsed = 00:00:16 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2757 ; free virtual = 72422default:defaulth px� 
}

Phase %s%s
101*constraints2
1.3 2default:default2.
Build Placer Netlist Model2default:defaultZ18-101h px� 
P
;Phase 1.3 Build Placer Netlist Model | Checksum: 197114364
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:01:19 ; elapsed = 00:00:38 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2663 ; free virtual = 71492default:defaulth px� 
z

Phase %s%s
101*constraints2
1.4 2default:default2+
Constrain Clocks/Macros2default:defaultZ18-101h px� 
M
8Phase 1.4 Constrain Clocks/Macros | Checksum: 197114364
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:01:19 ; elapsed = 00:00:39 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2665 ; free virtual = 71512default:defaulth px� 
I
4Phase 1 Placer Initialization | Checksum: 197114364
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:01:20 ; elapsed = 00:00:39 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2664 ; free virtual = 71502default:defaulth px� 
q

Phase %s%s
101*constraints2
2 2default:default2$
Global Placement2default:defaultZ18-101h px� 
p

Phase %s%s
101*constraints2
2.1 2default:default2!
Floorplanning2default:defaultZ18-101h px� 
C
.Phase 2.1 Floorplanning | Checksum: 17efa2480
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:02:29 ; elapsed = 00:01:10 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2606 ; free virtual = 70932default:defaulth px� 
x

Phase %s%s
101*constraints2
2.2 2default:default2)
Global Placement Core2default:defaultZ18-101h px� 
�

Phase %s%s
101*constraints2
2.2.1 2default:default20
Physical Synthesis In Placer2default:defaultZ18-101h px� 
K
)No high fanout nets found in the design.
65*physynthZ32-65h px� 
�
$Optimized %s %s. Created %s new %s.
216*physynth2
02default:default2
net2default:default2
02default:default2
instance2default:defaultZ32-232h px� 
�
aEnd %s Pass. Optimized %s %s. Created %s new %s, deleted %s existing %s and moved %s existing %s
415*physynth2
12default:default2
02default:default2
net or cell2default:default2
02default:default2
cell2default:default2
02default:default2
cell2default:default2
02default:default2
cell2default:defaultZ32-775h px� 
�
=Pass %s. Identified %s candidate %s for fanout optimization.
76*physynth2
12default:default2
12default:default2
net2default:defaultZ32-76h px� 
�
'Processed net %s. Replicated %s times.
81*physynth2�
Enutshell_i/NutShell_0/inst/nutcore/Cache_1/r_1_waymask_reg[2]_rep_n_0Enutshell_i/NutShell_0/inst/nutcore/Cache_1/r_1_waymask_reg[2]_rep_n_02default:default2
82default:default8Z32-81h px� 
�
$Optimized %s %s. Created %s new %s.
216*physynth2
12default:default2
net2default:default2
82default:default2
	instances2default:defaultZ32-232h px� 
�
aEnd %s Pass. Optimized %s %s. Created %s new %s, deleted %s existing %s and moved %s existing %s
415*physynth2
12default:default2
12default:default2
net or cell2default:default2
82default:default2
cells2default:default2
02default:default2
cell2default:default2
02default:default2
cell2default:defaultZ32-775h px� 
�
r%sTime (s): cpu = %s ; elapsed = %s . Memory (MB): peak = %s ; gain = %s ; free physical = %s ; free virtual = %s
480*common2.
Netlist sorting complete. 2default:default2
00:00:00.102default:default2
00:00:00.102default:default2
4974.3162default:default2
0.0002default:default2
25962default:default2
70872default:defaultZ17-722h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][0]Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][0]2default:default2�
Gnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_11	Gnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_112default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[0]Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[0]2default:default2�
Cnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_21	Cnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_212default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][1]Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][1]2default:default2�
Gnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_10	Gnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_102default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[1]Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[1]2default:default2�
Cnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_20	Cnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_202default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][2]Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][2]2default:default2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_9	Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_92default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[2]Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[2]2default:default2�
Cnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_19	Cnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_192default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][8]Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][8]2default:default2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_3	Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_32default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[8]Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[8]2default:default2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_13__0	Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_13__02default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][6]Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][6]2default:default2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_5	Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_52default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[6]Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[6]2default:default2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_15__0	Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_15__02default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][7]Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][7]2default:default2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_4	Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_42default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[7]Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[7]2default:default2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_14__0	Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_14__02default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][9]Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][9]2default:default2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_2	Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_22default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[9]Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[9]2default:default2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_12__0	Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_12__02default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][3]Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][3]2default:default2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_8	Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_82default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[3]Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[3]2default:default2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_18__0	Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_18__02default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][4]Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][4]2default:default2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_7	Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_72default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[4]Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[4]2default:default2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_17__0	Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_17__02default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][5]Enutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/r_1_req_addr_reg[12][5]2default:default2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_6	Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram/ram_reg_0_bram_0_i_62default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[5]Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_36[5]2default:default2�
Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_16__0	Fnutshell_i/NutShell_0/inst/nutcore/Cache_1/s3/ram_reg_0_bram_0_i_16__02default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src2[15]Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src2[15]2default:default2�
Cnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_2_reg__0_i_2	Cnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_2_reg__0_i_22default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src2[18]Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src2[18]2default:default2�
Anutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_2_reg_i_17	Anutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_2_reg_i_172default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src1[17]Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src1[17]2default:default2�
Anutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_20__0_i_17	Anutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_20__0_i_172default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src1[21]Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src1[21]2default:default2�
Anutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_20__0_i_13	Anutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_20__0_i_132default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src2[16]Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src2[16]2default:default2�
Cnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_2_reg__0_i_1	Cnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_2_reg__0_i_12default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src2[23]Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src2[23]2default:default2�
Anutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_2_reg_i_12	Anutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_2_reg_i_122default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src1[22]Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src1[22]2default:default2�
Anutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_20__0_i_12	Anutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_20__0_i_122default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src1[18]Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src1[18]2default:default2�
Anutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_20__0_i_16	Anutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_20__0_i_162default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Onutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src2[2]Onutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src2[2]2default:default2�
Dnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_2_reg__0_i_15	Dnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_2_reg__0_i_152default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src1[19]Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src1[19]2default:default2�
Anutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_20__0_i_15	Anutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_20__0_i_152default:default8Z32-117h px� 
�
DNet %s could not be optimized because driver %s could not be cloned
117*physynth2�
Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src2[20]Pnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/isu_io_out_bits_data_src2[20]2default:default2�
Anutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_2_reg_i_15	Anutshell_i/NutShell_0/inst/nutcore/Backend_inorder/REG_2_reg_i_152default:default8Z32-117h px� 
P
.No nets found for critical-cell optimization.
68*physynthZ32-68h px� 
�
$Optimized %s %s. Created %s new %s.
216*physynth2
02default:default2
net2default:default2
02default:default2
instance2default:defaultZ32-232h px� 
�
aEnd %s Pass. Optimized %s %s. Created %s new %s, deleted %s existing %s and moved %s existing %s
415*physynth2
12default:default2
02default:default2
net or cell2default:default2
02default:default2
cell2default:default2
02default:default2
cell2default:default2
02default:default2
cell2default:defaultZ32-775h px� 
�
CPass %s. Identified %s candidate %s for DSP register optimization.
275*physynth2
12default:default2
162default:default2
cells2default:defaultZ32-457h px� 
�
#Processed cell %s. %s %s pushed %s.339*physynth2�
Knutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_3_reg__4	Knutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_3_reg__42default:default2
172default:default2"
registers were2default:default2
out2default:default8Z32-665h px� 
�
#Processed cell %s. %s %s pushed %s.339*physynth2�
Knutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_2_reg__0	Knutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_2_reg__02default:default2
172default:default2"
registers were2default:default2
out2default:default8Z32-665h px� 
�
#Processed cell %s. %s %s pushed %s.339*physynth2�
Knutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_3_reg__3	Knutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_3_reg__32default:default2
172default:default2"
registers were2default:default2
out2default:default8Z32-665h px� 
�
#Processed cell %s. %s %s pushed %s.339*physynth2�
Knutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_2_reg__3	Knutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_2_reg__32default:default2
172default:default2"
registers were2default:default2
out2default:default8Z32-665h px� 
�
#Processed cell %s. %s %s pushed %s.339*physynth2�
Hnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_20__3	Hnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_20__32default:default2
172default:default2"
registers were2default:default2
out2default:default8Z32-665h px� 
�
#Processed cell %s. %s %s pushed %s.339*physynth2�
Knutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_2_reg__1	Knutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_2_reg__12default:default2
172default:default2"
registers were2default:default2
out2default:default8Z32-665h px� 
�
#Processed cell %s. %s %s pushed %s.339*physynth2�
Knutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_3_reg__2	Knutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_3_reg__22default:default2
172default:default2"
registers were2default:default2
out2default:default8Z32-665h px� 
�
#Processed cell %s. %s %s pushed %s.339*physynth2�
Knutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_2_reg__3	Knutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_2_reg__32default:default2
172default:default2"
registers were2default:default2
out2default:default8Z32-665h px� 
�
#Processed cell %s. %s %s pushed %s.339*physynth2�
Hnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_20__3	Hnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_20__32default:default2
172default:default2"
registers were2default:default2
out2default:default8Z32-665h px� 
�
#Processed cell %s. %s %s pushed %s.339*physynth2�
Knutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_2_reg__2	Knutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_2_reg__22default:default2
172default:default2"
registers were2default:default2
out2default:default8Z32-665h px� 
�
#Processed cell %s. %s %s pushed %s.339*physynth2�
Hnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_20__0	Hnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_20__02default:default2
172default:default2"
registers were2default:default2
out2default:default8Z32-665h px� 
�
#Processed cell %s. %s %s pushed %s.339*physynth2�
Hnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_20__2	Hnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_20__22default:default2
172default:default2"
registers were2default:default2
out2default:default8Z32-665h px� 
�
#Processed cell %s. %s %s pushed %s.339*physynth2�
Hnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_2_reg	Hnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_2_reg2default:default2
172default:default2"
registers were2default:default2
out2default:default8Z32-665h px� 
�
#Processed cell %s. %s %s pushed %s.339*physynth2�
Hnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_20__2	Hnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_20__22default:default2
172default:default2"
registers were2default:default2
out2default:default8Z32-665h px� 
�
#Processed cell %s. %s %s pushed %s.339*physynth2�
Hnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_20__1	Hnutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_20__12default:default2
172default:default2"
registers were2default:default2
out2default:default8Z32-665h px� 
�
#Processed cell %s. %s %s pushed %s.339*physynth2�
Knutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_3_reg__4	Knutshell_i/NutShell_0/inst/nutcore/Backend_inorder/exu/mdu/mul/REG_3_reg__42default:default2
172default:default2"
registers were2default:default2
out2default:default8Z32-665h px� 
�
aEnd %s Pass. Optimized %s %s. Created %s new %s, deleted %s existing %s and moved %s existing %s
415*physynth2
22default:default2
162default:default2!
nets or cells2default:default2
2722default:default2
cells2default:default2
02default:default2
cell2default:default2
02default:default2
cell2default:defaultZ32-775h px� 
�
r%sTime (s): cpu = %s ; elapsed = %s . Memory (MB): peak = %s ; gain = %s ; free physical = %s ; free virtual = %s
480*common2.
Netlist sorting complete. 2default:default2
00:00:00.102default:default2
00:00:00.112default:default2
4974.3162default:default2
0.0002default:default2
25952default:default2
70862default:defaultZ17-722h px� 
h
DNo candidate cells for SRL register optimization found in the design349*physynthZ32-677h px� 
�
aEnd %s Pass. Optimized %s %s. Created %s new %s, deleted %s existing %s and moved %s existing %s
415*physynth2
12default:default2
02default:default2
net or cell2default:default2
02default:default2
cell2default:default2
02default:default2
cell2default:default2
02default:default2
cell2default:defaultZ32-775h px� 
i
ENo candidate cells for BRAM register optimization found in the design297*physynthZ32-526h px� 
�
aEnd %s Pass. Optimized %s %s. Created %s new %s, deleted %s existing %s and moved %s existing %s
415*physynth2
12default:default2
02default:default2
net or cell2default:default2
02default:default2
cell2default:default2
02default:default2
cell2default:default2
02default:default2
cell2default:defaultZ32-775h px� 
R
.No candidate nets found for HD net replication521*physynthZ32-949h px� 
�
aEnd %s Pass. Optimized %s %s. Created %s new %s, deleted %s existing %s and moved %s existing %s
415*physynth2
12default:default2
02default:default2
net or cell2default:default2
02default:default2
cell2default:default2
02default:default2
cell2default:default2
02default:default2
cell2default:defaultZ32-775h px� 
�
r%sTime (s): cpu = %s ; elapsed = %s . Memory (MB): peak = %s ; gain = %s ; free physical = %s ; free virtual = %s
480*common2.
Netlist sorting complete. 2default:default2
00:00:00.022default:default2
00:00:00.022default:default2
4974.3162default:default2
0.0002default:default2
25972default:default2
70882default:defaultZ17-722h px� 
B
-
Summary of Physical Synthesis Optimizations
*commonh px� 
B
-============================================
*commonh px� 


*commonh px� 


*commonh px� 
�
�----------------------------------------------------------------------------------------------------------------------------------------
*commonh px� 
�
�|  Optimization                  |  Added Cells  |  Removed Cells  |  Optimized Cells/Nets  |  Dont Touch  |  Iterations  |  Elapsed   |
----------------------------------------------------------------------------------------------------------------------------------------
*commonh px� 
�	
�	|  Very High Fanout              |            0  |              0  |                     0  |           0  |           1  |  00:00:01  |
|  Fanout                        |            8  |              0  |                     1  |           0  |           1  |  00:00:00  |
|  Critical Cell                 |            0  |              0  |                     0  |           0  |           1  |  00:00:00  |
|  DSP Register                  |          272  |              0  |                    16  |           0  |           1  |  00:00:07  |
|  Shift Register                |            0  |              0  |                     0  |           0  |           1  |  00:00:00  |
|  BRAM Register                 |            0  |              0  |                     0  |           0  |           1  |  00:00:00  |
|  HD Interface Net Replication  |            0  |              0  |                     0  |           0  |           1  |  00:00:00  |
|  Total                         |          280  |              0  |                    17  |           0  |           7  |  00:00:08  |
----------------------------------------------------------------------------------------------------------------------------------------
*commonh px� 


*commonh px� 


*commonh px� 
T
?Phase 2.2.1 Physical Synthesis In Placer | Checksum: 1541bda26
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:05:04 ; elapsed = 00:02:37 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2595 ; free virtual = 70862default:defaulth px� 
J
5Phase 2.2 Global Placement Core | Checksum: fd2119ac
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:05:09 ; elapsed = 00:02:39 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2590 ; free virtual = 70822default:defaulth px� 
C
.Phase 2 Global Placement | Checksum: fd2119ac
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:05:09 ; elapsed = 00:02:39 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2619 ; free virtual = 71102default:defaulth px� 
q

Phase %s%s
101*constraints2
3 2default:default2$
Detail Placement2default:defaultZ18-101h px� 
}

Phase %s%s
101*constraints2
3.1 2default:default2.
Commit Multi Column Macros2default:defaultZ18-101h px� 
O
:Phase 3.1 Commit Multi Column Macros | Checksum: eb9f65e2
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:05:18 ; elapsed = 00:02:44 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2609 ; free virtual = 71002default:defaulth px� 


Phase %s%s
101*constraints2
3.2 2default:default20
Commit Most Macros & LUTRAMs2default:defaultZ18-101h px� 
R
=Phase 3.2 Commit Most Macros & LUTRAMs | Checksum: 197759599
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:05:31 ; elapsed = 00:02:50 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2612 ; free virtual = 71042default:defaulth px� 
y

Phase %s%s
101*constraints2
3.3 2default:default2*
Area Swap Optimization2default:defaultZ18-101h px� 
L
7Phase 3.3 Area Swap Optimization | Checksum: 14e0d6549
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:05:32 ; elapsed = 00:02:51 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2612 ; free virtual = 71042default:defaulth px� 
s

Phase %s%s
101*constraints2
3.4 2default:default2$
IO Cut Optimizer2default:defaultZ18-101h px� 
F
1Phase 3.4 IO Cut Optimizer | Checksum: 14e0d6549
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:05:33 ; elapsed = 00:02:52 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2612 ; free virtual = 71032default:defaulth px� 
t

Phase %s%s
101*constraints2
3.5 2default:default2%
Fast Optimization2default:defaultZ18-101h px� 
G
2Phase 3.5 Fast Optimization | Checksum: 1676440de
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:05:47 ; elapsed = 00:02:57 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2612 ; free virtual = 71032default:defaulth px� 
y

Phase %s%s
101*constraints2
3.6 2default:default2*
Small Shape Clustering2default:defaultZ18-101h px� 
L
7Phase 3.6 Small Shape Clustering | Checksum: 1686e46b9
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:05:54 ; elapsed = 00:03:03 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2572 ; free virtual = 70642default:defaulth px� 


Phase %s%s
101*constraints2
3.7 2default:default20
Flow Legalize Slice Clusters2default:defaultZ18-101h px� 
R
=Phase 3.7 Flow Legalize Slice Clusters | Checksum: 1a07fa0b0
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:05:54 ; elapsed = 00:03:04 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2574 ; free virtual = 70662default:defaulth px� 
r

Phase %s%s
101*constraints2
3.8 2default:default2#
Slice Area Swap2default:defaultZ18-101h px� 
D
/Phase 3.8 Slice Area Swap | Checksum: d8905743
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:06:08 ; elapsed = 00:03:17 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2556 ; free virtual = 70482default:defaulth px� 
x

Phase %s%s
101*constraints2
3.9 2default:default2)
Commit Slice Clusters2default:defaultZ18-101h px� 
J
5Phase 3.9 Commit Slice Clusters | Checksum: 71c686ff
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:06:37 ; elapsed = 00:03:26 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2543 ; free virtual = 70352default:defaulth px� 
v

Phase %s%s
101*constraints2
3.10 2default:default2&
Re-assign LUT pins2default:defaultZ18-101h px� 
H
3Phase 3.10 Re-assign LUT pins | Checksum: 2cfd7548
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:06:45 ; elapsed = 00:03:33 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2550 ; free virtual = 70422default:defaulth px� 
�

Phase %s%s
101*constraints2
3.11 2default:default22
Pipeline Register Optimization2default:defaultZ18-101h px� 
U
@Phase 3.11 Pipeline Register Optimization | Checksum: 180a8b583
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:06:46 ; elapsed = 00:03:34 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2564 ; free virtual = 70562default:defaulth px� 
D
/Phase 3 Detail Placement | Checksum: 180a8b583
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:06:46 ; elapsed = 00:03:35 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2564 ; free virtual = 70562default:defaulth px� 
�

Phase %s%s
101*constraints2
4 2default:default2<
(Post Placement Optimization and Clean-Up2default:defaultZ18-101h px� 
{

Phase %s%s
101*constraints2
4.1 2default:default2,
Post Commit Optimization2default:defaultZ18-101h px� 
E
%Done setting XDC timing constraints.
35*timingZ38-35h px� 
�

Phase %s%s
101*constraints2
4.1.1 2default:default2/
Post Placement Optimization2default:defaultZ18-101h px� 
V
APost Placement Optimization Initialization | Checksum: 1d260a4af
*commonh px� 
u

Phase %s%s
101*constraints2
4.1.1.1 2default:default2"
BUFG Insertion2default:defaultZ18-101h px� 
�
2Processed net %s, inserted BUFG to drive %s loads.34*	placeflow2-
corerstn_sync_reg_n_0_[1]2default:default2
44812default:defaultZ46-35h px� 
o
Replicated bufg driver %s39*	placeflow21
corerstn_sync_reg[1]_bufg_rep2default:defaultZ46-45h px� 
�
2Processed net %s, inserted BUFG to drive %s loads.34*	placeflow2�
�zynq_soc_i/hier_rvcore_peripheral/axi_dma_rvcore/U0/I_RST_MODULE/GEN_RESET_FOR_S2MM.RESET_I/GEN_ASYNC_RESET.scndry_resetn_reg_0[0]2default:default2
11892default:defaultZ46-35h px� 
�
�BUFG insertion identified %s candidate nets. Inserted BUFG: %s, Replicated BUFG Driver: %s, Skipped due to Placement/Routing Conflicts: %s, Skipped due to Timing Degradation: %s, Skipped due to Illegal Netlist: %s.43*	placeflow2
22default:default2
22default:default2
12default:default2
02default:default2
02default:default2
02default:defaultZ46-56h px� 
H
3Phase 4.1.1.1 BUFG Insertion | Checksum: 1d1f880a8
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:07:49 ; elapsed = 00:03:59 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2592 ; free virtual = 70842default:defaulth px� 
�
hPost Placement Timing Summary WNS=%s. For the most accurate timing information please run report_timing.610*place2
0.6812default:defaultZ30-746h px� 
S
>Phase 4.1.1 Post Placement Optimization | Checksum: 2a4156f32
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:07:50 ; elapsed = 00:04:00 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2592 ; free virtual = 70842default:defaulth px� 
N
9Phase 4.1 Post Commit Optimization | Checksum: 2a4156f32
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:07:51 ; elapsed = 00:04:01 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2592 ; free virtual = 70842default:defaulth px� 
y

Phase %s%s
101*constraints2
4.2 2default:default2*
Post Placement Cleanup2default:defaultZ18-101h px� 
L
7Phase 4.2 Post Placement Cleanup | Checksum: 2a4156f32
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:07:52 ; elapsed = 00:04:02 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2593 ; free virtual = 70852default:defaulth px� 
�
r%sTime (s): cpu = %s ; elapsed = %s . Memory (MB): peak = %s ; gain = %s ; free physical = %s ; free virtual = %s
480*common2.
Netlist sorting complete. 2default:default2
00:00:00.372default:default2
00:00:00.372default:default2
4974.3162default:default2
0.0002default:default2
25922default:default2
70842default:defaultZ17-722h px� 
s

Phase %s%s
101*constraints2
4.3 2default:default2$
Placer Reporting2default:defaultZ18-101h px� 
F
1Phase 4.3 Placer Reporting | Checksum: 38ffe4191
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:07:58 ; elapsed = 00:04:08 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2598 ; free virtual = 70902default:defaulth px� 
z

Phase %s%s
101*constraints2
4.4 2default:default2+
Final Placement Cleanup2default:defaultZ18-101h px� 
�
r%sTime (s): cpu = %s ; elapsed = %s . Memory (MB): peak = %s ; gain = %s ; free physical = %s ; free virtual = %s
480*common2.
Netlist sorting complete. 2default:default2
00:00:00.012default:default2
00:00:00.022default:default2
4974.3162default:default2
0.0002default:default2
25982default:default2
70902default:defaultZ17-722h px� 
M
8Phase 4.4 Final Placement Cleanup | Checksum: 35cfdbe6a
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:07:59 ; elapsed = 00:04:09 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2598 ; free virtual = 70902default:defaulth px� 
\
GPhase 4 Post Placement Optimization and Clean-Up | Checksum: 35cfdbe6a
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:07:59 ; elapsed = 00:04:10 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2598 ; free virtual = 70902default:defaulth px� 
>
)Ending Placer Task | Checksum: 2878da2c4
*commonh px� 
�

%s
*constraints2�
�Time (s): cpu = 00:07:59 ; elapsed = 00:04:10 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2598 ; free virtual = 70902default:defaulth px� 
Z
Releasing license: %s
83*common2"
Implementation2default:defaultZ17-83h px� 
�
G%s Infos, %s Warnings, %s Critical Warnings and %s Errors encountered.
28*	vivadotcl2
1772default:default2
102default:default2
02default:default2
02default:defaultZ4-41h px� 
^
%s completed successfully
29*	vivadotcl2 
place_design2default:defaultZ4-42h px� 
�
r%sTime (s): cpu = %s ; elapsed = %s . Memory (MB): peak = %s ; gain = %s ; free physical = %s ; free virtual = %s
480*common2"
place_design: 2default:default2
00:08:132default:default2
00:04:152default:default2
4974.3162default:default2
0.0002default:default2
26652default:default2
71572default:defaultZ17-722h px� 
�
r%sTime (s): cpu = %s ; elapsed = %s . Memory (MB): peak = %s ; gain = %s ; free physical = %s ; free virtual = %s
480*common2.
Netlist sorting complete. 2default:default2
00:00:00.012default:default2
00:00:00.022default:default2
4974.3162default:default2
0.0002default:default2
26662default:default2
71582default:defaultZ17-722h px� 
H
&Writing timing data to binary archive.266*timingZ38-480h px� 
D
Writing placer database...
1603*designutilsZ20-1893h px� 
=
Writing XDEF routing.
211*designutilsZ20-211h px� 
J
#Writing XDEF routing logical nets.
209*designutilsZ20-209h px� 
J
#Writing XDEF routing special nets.
210*designutilsZ20-210h px� 
�
r%sTime (s): cpu = %s ; elapsed = %s . Memory (MB): peak = %s ; gain = %s ; free physical = %s ; free virtual = %s
480*common2)
Write XDEF Complete: 2default:default2
00:00:202default:default2
00:00:062default:default2
4974.3162default:default2
0.0002default:default2
25522default:default2
71292default:defaultZ17-722h px� 
�
 The %s '%s' has been generated.
621*common2

checkpoint2default:default2�
r/home/stu/NutShell/fpga/board/axu3cg/build/onboardtest-axu3cg/onboardtest-axu3cg.runs/impl_1/system_top_placed.dcp2default:defaultZ17-1381h px� 
�
r%sTime (s): cpu = %s ; elapsed = %s . Memory (MB): peak = %s ; gain = %s ; free physical = %s ; free virtual = %s
480*common2&
write_checkpoint: 2default:default2
00:00:292default:default2
00:00:152default:default2
4974.3162default:default2
0.0002default:default2
26362default:default2
71512default:defaultZ17-722h px� 
e
%s4*runtcl2I
5Executing : report_io -file system_top_io_placed.rpt
2default:defaulth px� 
�
�report_io: Time (s): cpu = 00:00:00.38 ; elapsed = 00:00:00.46 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2618 ; free virtual = 7134
*commonh px� 
�
%s4*runtcl2�
lExecuting : report_utilization -file system_top_utilization_placed.rpt -pb system_top_utilization_placed.pb
2default:defaulth px� 
�
r%sTime (s): cpu = %s ; elapsed = %s . Memory (MB): peak = %s ; gain = %s ; free physical = %s ; free virtual = %s
480*common2(
report_utilization: 2default:default2
00:00:272default:default2
00:00:202default:default2
4974.3162default:default2
0.0002default:default2
26382default:default2
71542default:defaultZ17-722h px� 
�
%s4*runtcl2f
RExecuting : report_control_sets -verbose -file system_top_control_sets_placed.rpt
2default:defaulth px� 
�
�report_control_sets: Time (s): cpu = 00:00:03 ; elapsed = 00:00:03 . Memory (MB): peak = 4974.316 ; gain = 0.000 ; free physical = 2635 ; free virtual = 7152
*commonh px� 


End Record