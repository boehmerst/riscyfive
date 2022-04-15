onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /proxy_tb/dut/clk_i
add wave -noupdate /proxy_tb/dut/reset_n_i
add wave -noupdate /proxy_tb/dut/sim_done_i
add wave -noupdate -expand -subitemconfig {/proxy_tb/dut/host2mem_req_o.bits -expand} /proxy_tb/dut/host2mem_req_o
add wave -noupdate /proxy_tb/dut/host2mem_rsp_i
add wave -noupdate /proxy_tb/dut/mem2host_req_i
add wave -noupdate /proxy_tb/dut/mem2host_rsp_o
add wave -noupdate -expand -subitemconfig {/proxy_tb/dut/host2target_req_o.bits -expand} /proxy_tb/dut/host2target_req_o
add wave -noupdate /proxy_tb/dut/host2target_rsp_i
add wave -noupdate /proxy_tb/dut/target2host_req_i
add wave -noupdate /proxy_tb/dut/target2host_rsp_o
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {27512748 ns} 0}
quietly wave cursor active 1
configure wave -namecolwidth 194
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
WaveRestoreZoom {0 ns} {103623833 ns}
