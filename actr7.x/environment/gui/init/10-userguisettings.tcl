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
  set window_config(.param_viewer) 1033x587+1080+570
  set changed_window_list(.param_viewer) 1
  set window_config(.dispatcher) 700x274+930+583
  set changed_window_list(.dispatcher) 1
  set window_config(.stepper) 792x524+74+1010
  set changed_window_list(.stepper) 1
  set window_config(.visicon) 1460x353+917+124
  set changed_window_list(.visicon) 1
  set window_config(.options) 450x274+1055+583
  set changed_window_list(.options) 1
  set window_config(.control_panel) 415x1197+2070+249
  set changed_window_list(.control_panel) 1
  set window_config(.declarative) 834x577+432+598
  set changed_window_list(.declarative) 1
  set window_config(.buffers) 470x240+1974+403
  set changed_window_list(.buffers) 1
  set window_config(.copyright) 400x290+1080+575
  set changed_window_list(.copyright) 1
  set window_config(.procedural) 500x400+1030+520
  set changed_window_list(.procedural) 1
}
set gui_options(p_selected) #44DA22
set gui_options(p_matched) #FCA31D
set gui_options(p_mismatched) #E1031E
