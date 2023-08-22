$PBExportHeader$w_valida_contenedor.srw
forward
global type w_valida_contenedor from window
end type
type em_contenedor1 from editmask within w_valida_contenedor
end type
type st_4 from statictext within w_valida_contenedor
end type
type st_1 from statictext within w_valida_contenedor
end type
type pb_salir from picturebutton within w_valida_contenedor
end type
type gb_1 from groupbox within w_valida_contenedor
end type
end forward

global type w_valida_contenedor from window
integer width = 2117
integer height = 364
boolean titlebar = true
string title = "Validación"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 30586022
string icon = "TABLA.ICO"
event ue_guardar pbm_custom11
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
event ue_genera_facturas pbm_custom04
em_contenedor1 em_contenedor1
st_4 st_4
st_1 st_1
pb_salir pb_salir
gb_1 gb_1
end type
global w_valida_contenedor w_valida_contenedor

type variables
Boolean			ib_Anulacion
Integer			ii_PlantaSag, ii_CantTarjas, ii_CantInspec
Date				id_FechaRepa
Date		id_FechaAcceso
Time		it_HoraAcceso


end variables

forward prototypes
public function boolean existe_termografo (string as_termografo)
end prototypes

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

public function boolean existe_termografo (string as_termografo);Long		li_existe

SELECT COUNT()
	INTO :li_existe
	FROM DBA.termografo
	WHERE term_codigo = :as_termografo
	AND	term_tipter = 2
	AND	term_estado = 1;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla despafrigoen")
	RETURN True
END IF	

IF li_existe = 0 THEN
	
	MessageBox("Atención","Termógrafo no Existe o se Encuentra en Tránsito", Exclamation!, Ok!)
	RETURN True
ELSE	
	RETURN False
END IF	
end function

on w_valida_contenedor.create
this.em_contenedor1=create em_contenedor1
this.st_4=create st_4
this.st_1=create st_1
this.pb_salir=create pb_salir
this.gb_1=create gb_1
this.Control[]={this.em_contenedor1,&
this.st_4,&
this.st_1,&
this.pb_salir,&
this.gb_1}
end on

on w_valida_contenedor.destroy
destroy(this.em_contenedor1)
destroy(this.st_4)
destroy(this.st_1)
destroy(this.pb_salir)
destroy(this.gb_1)
end on

event open;x = 500
y = 1400

This.Icon	=	Gstr_apl.Icono


end event

type em_contenedor1 from editmask within w_valida_contenedor
integer x = 768
integer y = 96
integer width = 777
integer height = 100
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "aaaaaaaaaaaaaaa"
end type

event modified;IF This.Text <> istr_mant3.argumento[1] AND This.Text <> '' THEN
	MessageBox("Atención","N° Contenedor no coincide con el ingresado", Exclamation!, Ok!)
	This.Text = ''
	This.SetFocus()
	RETURN 1
ELSE
	pb_salir.TriggerEvent(Clicked!)
END IF	
end event

type st_4 from statictext within w_valida_contenedor
integer x = 96
integer y = 120
integer width = 585
integer height = 88
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Repita Contenedor"
boolean focusrectangle = false
end type

type st_1 from statictext within w_valida_contenedor
integer x = 32
integer y = 64
integer width = 1678
integer height = 176
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_valida_contenedor
integer x = 1774
integer y = 48
integer width = 238
integer height = 200
integer taborder = 30
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "F:\Desarrollo 12\Imagenes\Botones\SalirEnab.png"
alignment htextalign = left!
end type

event clicked;IF em_contenedor1.Text = '' THEN
	istr_mant3.argumento[5] = ''
ELSE
	istr_mant3.argumento[5] = '1212'
END IF	

CloseWithReturn(Parent, istr_mant3)

end event

type gb_1 from groupbox within w_valida_contenedor
integer x = 1755
integer width = 274
integer height = 268
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

