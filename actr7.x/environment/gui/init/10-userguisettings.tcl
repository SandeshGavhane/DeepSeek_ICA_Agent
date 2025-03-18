# this file generated when environment is closed
# refresh . to make sure sizes are right

wm deiconify .
update
wm withdraw .
if {[winfo screenwidth .] != 2560 || [winfo screenheight .] != 1440 || [lindex [wm maxsize .] 0] != 2564 || [lindex [wm maxsize .] 1] != 1421} {
  set size_mismatch 1
} else {
  set size_mismatch 0
}

if $size_mismatch {
  set reset_window_sizes [tk_messageBox -icon warning -title "Screen resolution changed" -type yesno \
                                         -message "The screen resolution is not the same as it was the last time the Environment was used.  Should the window positions reset to the defaults?"]
} else { set reset_window_sizes 0}
if {$reset_window_sizes != "yes"} {
  set window_config(.visicon) 1773x302+247+701
  set changed_window_list(.visicon) 1
  set window_config(.buffer_history) 530x290+1015+620
  set changed_window_list(.buffer_history) 1
  set window_config(.visicon_history) 810x340+875+550
  set changed_window_list(.visicon_history) 1
  set window_config(.buffers) 760x250+362+890
  set changed_window_list(.buffers) 1
  set window_config(.retrieval_history) 670x380+1233+827
  set changed_window_list(.retrieval_history) 1
  set window_config(.ptrace) 1027x412+1075+570
  set changed_window_list(.ptrace) 1
  set window_config(.control_panel) 235x946+2220+371
  set changed_window_list(.control_panel) 1
  set window_config(.param_viewer) 509x467+767+772
  set changed_window_list(.param_viewer) 1
  set window_config(.stepper) 784x562+1049+152
  set changed_window_list(.stepper) 1
  set window_config(.declarative) 705x589+285+79
  set changed_window_list(.declarative) 1
  set window_config(.reload_response) 500x230+1030+605
  set changed_window_list(.reload_response) 1
  set window_config(.event_queue) 800x180+880+630
  set changed_window_list(.event_queue) 1
  set window_config(.procedural) 631x572+378+937
  set changed_window_list(.procedural) 1
  set window_config(.copyright) 400x290+1080+575
  set changed_window_list(.copyright) 1
}
set gui_options(p_selected) #44DA22
set gui_options(p_matched) #FCA31D
set gui_options(p_mismatched) #E1031E
