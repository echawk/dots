;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.

(add-to-load-path (dirname (current-filename)))

;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu)
	         (gnu services desktop)
             (gnu services xorg)

             (filesystems) ;; local filesystems.scm
	         (nonguix transformations)

             (gnu packages freedesktop)
             (gnu packages wm)
             (gnu packages xorg)

	         (nongnu packages linux)
	         (nongnu packages nvidia)
	         (nongnu system linux-initrd))

(use-service-modules desktop networking ssh xorg)

(define %nvda nvda-580)

(define (with-nvidia pkg)
  ((replace-mesa #:driver %nvda) pkg))

(define %the-os
  (operating-system
   (kernel linux)
   (initrd microcode-initrd)
   (firmware (list linux-firmware))

   (locale "en_US.utf8")
   (timezone "America/Indiana/Indianapolis")
   (keyboard-layout (keyboard-layout "us"))
   (host-name "guix-desktop")

   (packages (append
              (list egl-wayland)
              (map with-nvidia
                   (list xorg-server
                         xinit))
		      %base-packages))

   ;; The list of user accounts ('root' is implicit).
   (users (cons* (user-account
                  (name "ethan")
                  (comment "Ethan")
                  (group "users")
                  (home-directory "/home/ethan")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
		         %base-user-accounts))

   ;; Below is the list of system services.  To search for available
   ;; services, run 'guix system search KEYWORD' in a terminal.
   (services
    (append (list

             (set-xorg-configuration
              (xorg-configuration
               (server (with-nvidia xorg-server))
               (modules (cons %nvda %default-xorg-modules))
               (drivers '("nvidia"))
               (keyboard-layout keyboard-layout)))
             (service elogind-service-type)
             (service dbus-root-service-type)
             (service openssh-service-type)
             (service dhcpcd-service-type)
             (service ntp-service-type))
            %base-services))

   (bootloader (bootloader-configuration
		        (bootloader grub-efi-bootloader)
		        (targets (list "/boot/efi"))
		        (keyboard-layout keyboard-layout)))

   ;; The list of file systems that get "mounted".  The unique
   ;; file system identifiers there ("UUIDs") can be obtained
   ;; by running 'blkid' in a terminal.
   (file-systems %the-file-systems)))

;; Propreitary NVIDIA driver config; straight from nonguix repo.
((nonguix-transformation-nvidia
  #:driver %nvda
  #:open-source-kernel-module? #t
  #:configure-xorg? #f)
 %the-os)
;;%the-os
