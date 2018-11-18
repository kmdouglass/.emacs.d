(setq user-home (expand-file-name "~/"))

(ede-cpp-root-project "RPi-DeviceAdapters"
		      :file (concat (file-name-as-directory user-home) "src/RPi-DeviceAdapters/Makefile")
		      :include-path '("src/DeviceAdapters/RPiGPIO"
				      "src/DeviceAdapters/RPiTutorial"
				      "src/DeviceAdapters/RPiV4L2")
		      :system-include-path '("/opt/rpi-micromanager/micro-manager/MMDevice/"))
