onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -expand -group divider /serial_divider_tb/divi0/clk_i
add wave -noupdate -expand -group divider /serial_divider_tb/divi0/reset_n_i
add wave -noupdate -expand -group divider /serial_divider_tb/divi0/en_i
add wave -noupdate -expand -group divider /serial_divider_tb/divi0/init_i
add wave -noupdate -expand -group divider /serial_divider_tb/divi0/start_i
add wave -noupdate -expand -group divider /serial_divider_tb/divi0/signed_i
add wave -noupdate -expand -group divider /serial_divider_tb/divi0/dividend_i
add wave -noupdate -expand -group divider /serial_divider_tb/divi0/divider_i
add wave -noupdate -expand -group divider /serial_divider_tb/divi0/quotient_o
add wave -noupdate -expand -group divider /serial_divider_tb/divi0/reminder_o
add wave -noupdate -expand -group divider /serial_divider_tb/divi0/ready_o
add wave -noupdate -expand -group divider /serial_divider_tb/divi0/r
add wave -noupdate -expand -group divider /serial_divider_tb/divi0/rin
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {294 ns} 0}
quietly wave cursor active 1
configure wave -namecolwidth 150
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
WaveRestoreZoom {0 ns} {1 us}
