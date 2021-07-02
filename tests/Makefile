all: \
	coin_outlier_main.exe \
	coin_main.exe \
	gaussian_gaussian_main.exe \
	kalman_first_main.exe \
	kalman_fns_main.exe \
	kalman_generative_main.exe \
	kalman_helper_main.exe \
	kalman_multiple_ts_main.exe \
	kalman_normal_main.exe \
	mtt.check \
	outlier_main.exe \
	robot.check \
	slam_array.check \
	slam_tuple.check \
	slam.check \
	tuple_main.exe \
	up_multiple_its_main.exe

bench: \
	 kalman_normal.check \
	 kalman_first.check \
	 kalman_generative.check \
	 robot.check \
	 coin.check \
	 gaussian_gaussian.check \
	 outlier.check \
	 mtt.check \
	 slam_array.check


%.check: %.muf
	@mufc --only-check $<

%_main.exe: %.muf
	mufc $<

clean:
	rm -rf *.cm* *.ml *.exe

