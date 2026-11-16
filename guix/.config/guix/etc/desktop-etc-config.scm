;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu)
	     (gnu services desktop)

	     (nonguix transformations)
	     (nongnu packages linux)
	     (nongnu packages nvidia)
	     (nongnu system linux-initrd))

(use-service-modules desktop networking ssh xorg)

(define %the-os
  (operating-system
   (kernel linux)
   ;; (kernel-arguments
   ;;  (cons* "nvidia-drm.modeset=1"
   ;; 	  "modprobe.blacklist=nouveau"
   ;; 	  %default-kernel-arguments))
   ;; (kernel-loadable-modules (list nvidia-driver))
   
   (initrd microcode-initrd)
   
   (firmware (list linux-firmware))
   
   
   (locale "en_US.utf8")
   (timezone "America/Indiana/Indianapolis")
   (keyboard-layout (keyboard-layout "us"))
   (host-name "guix-desktop")

   ;; (packages (append (list mg)
   ;; 		   %base-packages))
   
   
   ;; The list of user accounts ('root' is implicit).
   (users (cons* (user-account
                  (name "ethan")
                  (comment "Ethan")
                  (group "users")
                  (home-directory "/home/ethan")
                  (supplementary-groups '("wheel" "netdev" "audio" "video" "seat")))
		 %base-user-accounts))

   ;; Below is the list of system services.  To search for available
   ;; services, run 'guix system search KEYWORD' in a terminal.
   (services
    (append (list
             ;; To configure OpenSSH, pass an 'openssh-configuration'
             ;; record as a second argument to 'service' below.
             (service seatd-service-type)
             (service openssh-service-type)
             (service dhcpcd-service-type)
             (service ntp-service-type))

            ;; This is the default list of services we
            ;; are appending to.
            %base-services))
   (bootloader (bootloader-configuration
		(bootloader grub-efi-bootloader)
		(targets (list "/boot/efi"))
		(keyboard-layout keyboard-layout)))
   (swap-devices (list (swap-space
			(target (uuid
				 "22cf5195-49ea-423a-b114-ee24833d20ef")))))

   ;; The list of file systems that get "mounted".  The unique
   ;; file system identifiers there ("UUIDs") can be obtained
   ;; by running 'blkid' in a terminal.
   (file-systems (cons* (file-system
			 (mount-point "/boot/efi")
			 (device (uuid "A340-F609"
                                       'fat32))
			 (type "vfat"))
			(file-system
			 (mount-point "/")
			 (device (uuid
                                  "3a44b652-3a06-427a-becb-3e0e12c501a8"
                                  'ext4))
			 (type "ext4")) %base-file-systems))))

;; Propreitary NVIDIA driver config; straight from nonguix repo.
;; ((nonguix-transformation-nvidia
;;   #:driver nvda-595
;;   #:configure-xorg? #t)
;;  %the-os)
%the-os
