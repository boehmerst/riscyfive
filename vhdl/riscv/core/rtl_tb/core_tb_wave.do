onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /core_tb/reset_n
add wave -noupdate /core_tb/clk
add wave -noupdate -group fetchi0 /core_tb/corei0/fetchi0/clk_i
add wave -noupdate -group fetchi0 /core_tb/corei0/fetchi0/reset_n_i
add wave -noupdate -group fetchi0 /core_tb/corei0/fetchi0/init_i
add wave -noupdate -group fetchi0 /core_tb/corei0/fetchi0/en_i
add wave -noupdate -group fetchi0 -expand /core_tb/corei0/fetchi0/fetch_i
add wave -noupdate -group fetchi0 /core_tb/corei0/fetchi0/fetch_o
add wave -noupdate -group fetchi0 /core_tb/corei0/fetchi0/imem_o
add wave -noupdate -group fetchi0 /core_tb/corei0/fetchi0/r
add wave -noupdate -group fetchi0 /core_tb/corei0/fetchi0/rin
add wave -noupdate -group decodei0 /core_tb/corei0/decodei0/clk_i
add wave -noupdate -group decodei0 /core_tb/corei0/decodei0/reset_n_i
add wave -noupdate -group decodei0 /core_tb/corei0/decodei0/init_i
add wave -noupdate -group decodei0 /core_tb/corei0/decodei0/en_i
add wave -noupdate -group decodei0 /core_tb/corei0/decodei0/decode_i
add wave -noupdate -group decodei0 /core_tb/corei0/decodei0/decode_o
add wave -noupdate -group decodei0 /core_tb/corei0/decodei0/decode_comb_o
add wave -noupdate -group decodei0 /core_tb/corei0/decodei0/gprf_o
add wave -noupdate -group decodei0 /core_tb/corei0/decodei0/gprf
add wave -noupdate -group decodei0 /core_tb/corei0/decodei0/wb_data_d
add wave -noupdate -group decodei0 /core_tb/corei0/decodei0/r
add wave -noupdate -group decodei0 /core_tb/corei0/decodei0/rin
add wave -noupdate -group execi0 /core_tb/corei0/execi0/clk_i
add wave -noupdate -group execi0 /core_tb/corei0/execi0/reset_n_i
add wave -noupdate -group execi0 /core_tb/corei0/execi0/init_i
add wave -noupdate -group execi0 /core_tb/corei0/execi0/en_i
add wave -noupdate -group execi0 /core_tb/corei0/execi0/exec_i
add wave -noupdate -group execi0 /core_tb/corei0/execi0/exec_o
add wave -noupdate -group execi0 /core_tb/corei0/execi0/exec_comb_o
add wave -noupdate -group execi0 /core_tb/corei0/execi0/status_o
add wave -noupdate -group execi0 /core_tb/corei0/execi0/ready_o
add wave -noupdate -group execi0 /core_tb/corei0/execi0/dividend
add wave -noupdate -group execi0 /core_tb/corei0/execi0/divider
add wave -noupdate -group execi0 /core_tb/corei0/execi0/div_blocki0_quotient
add wave -noupdate -group execi0 /core_tb/corei0/execi0/div_blocki0_reminder
add wave -noupdate -group execi0 /core_tb/corei0/execi0/div_blocki0_ready
add wave -noupdate -group execi0 /core_tb/corei0/execi0/ena
add wave -noupdate -group execi0 /core_tb/corei0/execi0/r
add wave -noupdate -group execi0 /core_tb/corei0/execi0/rin
add wave -noupdate -group memi0 /core_tb/corei0/memi0/clk_i
add wave -noupdate -group memi0 /core_tb/corei0/memi0/reset_n_i
add wave -noupdate -group memi0 /core_tb/corei0/memi0/init_i
add wave -noupdate -group memi0 /core_tb/corei0/memi0/en_i
add wave -noupdate -group memi0 /core_tb/corei0/memi0/mem_o
add wave -noupdate -group memi0 /core_tb/corei0/memi0/mem_i
add wave -noupdate -group memi0 -expand /core_tb/corei0/memi0/dmem_o
add wave -noupdate -group memi0 /core_tb/corei0/memi0/r
add wave -noupdate -group memi0 /core_tb/corei0/memi0/rin
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {4364 ns} 0}
quietly wave cursor active 1
configure wave -namecolwidth 232
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 1
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits sec
update
WaveRestoreZoom {4207 ns} {5042 ns}
