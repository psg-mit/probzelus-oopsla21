tests: \
	coin_outlier_main.exe \
	coin_main.exe \
	gaussian_gaussian_main.exe \
	kalman_first_main.exe \
	kalman_fns_main.exe \
	kalman_generative_main.exe \
	kalman_helper_main.exe \
	kalman_multiple_ts_main.exe \
	kalman_normal_main.exe \
	mtt_main.exe \
	outlier_main.exe \
	robot_main.exe \
	slam_array_main.exe \
	slam_tuple_main.exe \
	slam_main.exe \
	tuple_main.exe \
	up_multiple_its_main.exe \
	up_catchup_main.exe \
	up_catchup_delay_main.exe

bench: \
	 kalman_normal.check \
	 kalman_first.check \
	 kalman_generative.check \
	 robot.check \
	 coin.check \
	 gaussian_gaussian.check \
	 outlier.check \
	 mtt.check \
	 slam_array.check \
	 up_catchup.check \
	 up_catchup_delay.check


%.check: %.muf
	@mufc --only-check $<

%_main.exe: %.muf
	mufc $<

clean:
	rm -rf *.cm* *.ml *.exe

