device, GET_SCREEN_SIZE=screen_size

defsysv, "!DEVX", screen_size(0)-10
defsysv, "!DEVY", screen_size(1)-24
defsysv, "!DEBUG", 0

!PATH = "~/idl/vlib/widgets:"	+ $
	"~/idl/vlib/math:"	+ $
	"~/idl/vlib/image:"	+ $
	"~/idl/vlib/color:"	+ $
	"~/idl/vlib/window:"	+ $
	"~/idl/vlib/graphics:"	+ $
	"~/idl/vlib/struct:"	+ $
	"~/idl/vlib/misc:"	+ $
	"~/idl/vlib/io:"	+ $
	"~/idl/vlib/data:"	+ $
	"~/idl/vlibm/mgep:"	+ $
	"~/idl/vlibm/rtfit:"	+ $
	"~/idl/vlibm/models:"	+ $
	"~/idl/vlibm/deconv:"	+ !PATH

if (!D.name EQ "X") then !PATH = "~/idl/vlib/xdoc:" + !PATH

cv = {	CONSTANTS_CGS,	$
	h: 6.6255e-27	,$ ;Planck's constant, erg-sec.
	sb: 5.67e-5	,$ ;Stephan-Boltzman constant, erg/cm^2/deg^4.
	k: 1.381e-16	,$ ;Boltzman constant, erg/degree.
	wtmax: 2897.3	,$ ;micron-deg, wavelen * max( T ) of B-B spectrum.
	c: 2.997924e10	,$ ;cm/sec, speed of light in vacuum.
	ev: 1.602e-12	,$ ;ergs/eV
	g: 6.67e-8	,$ ;Gravitation constant, dynes/gm/cm^2
	mH: 1.66e-24	,$ ;mass of Hydrogen, gm.
	Lsun: 3.826e33	,$ ;solar luminosity, ergs/sec
	Msun: 1.989e33	,$ ;solar mass, gm.
	Rsun: 6.596e10	,$ ;solar radius, cm.
	a0: 5.292e-9	,$ ;Bohr radius, cm.
	txsec: 6.65e-25 ,$ ;Thomson cross section, cm^2.
	pc: 3.086e18	,$ ;parsec, cm.
	AU: 1.496e13	}  ;sun to earth distance, cm.

defsysv, "!CV", cv, 1	;define as read-only sys-var.

@~/idl/setup	;invoke a user defined global setup.

@setup	;invoke whatever setup is present in current directory,
	; otherwise, ignore the error message about file not found.
