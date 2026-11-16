(use-modules (gnu home)
	         (guix gexp)
	         (gnu home services)
	         (gnu home services desktop)
	         (gnu home services sound)
	         (gnu home services shells)

	         (gnu packages linux)

	         (gnu packages glib)

	         (gnu packages emacs)

             (gnu packages xorg)
             (gnu packages compton)
             (gnu packages xdisorg)
             (gnu packages suckless)
	         (gnu packages package-management)
	         (gnu packages terminals)
	         (gnu packages window-management)

	         (gnu packages chromium)

             (gnu packages rust-apps)
             (gnu packages gcc)
             (gnu packages llvm)
	         (gnu packages lisp)
	         (gnu packages racket)

	         (gnu packages admin))


(home-environment
 (services
  (append
   (list
    (simple-service 'inject-local-profile
		            home-shell-profile-service-type
		            (list
		             (plain-file "local-profile-injection"
		                         "
# Source local profile if it exists.
if [ -f $HOME/.profile.local ]; then
  . $HOME/.profile.local
fi
")))

    ;; (simple-service 'wayland-nvidia-env
    ;; 		    home-environment-variables-service-type
    ;; 		    `(("WLR_NO_HARDWARE_CURSORS" . "1")
    ;; 		      ("WLR_RENDERER" . "vulkan")
    ;; 		      ("XDG_SESSION_TYPE" . "wayland")))

    (service home-bash-service-type)
    (service home-dbus-service-type)
    (service home-pipewire-service-type))
   %base-home-services))
 (packages (list htop

		         dbus
		         pipewire

                 ;;		         flatpak

                 ;;                st
		         ;;     alacritty
		         ;;       labwc
                 ;;                 cage

                 ;; quickshell

                 ;;               niri foot fuzzel mako wl-clipboard


                 ;;             xorg-server sx picom xclip xset hsetroot setxkbmap sxhkd

		         ;; ungoogled-chromium

		         emacs

                 ;;gcc
                 ;;clang
		         sbcl
		         ;;racket

                 ;;tectonic
		         )))
