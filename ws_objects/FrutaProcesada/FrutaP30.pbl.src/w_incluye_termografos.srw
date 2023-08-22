$PBExportHeader$w_incluye_termografos.srw
forward
global type w_incluye_termografos from window
end type
type em_termografo3 from editmask within w_incluye_termografos
end type
type em_termografo2 from editmask within w_incluye_termografos
end type
type st_5 from statictext within w_incluye_termografos
end type
type st_4 from statictext within w_incluye_termografos
end type
type em_termografo1 from editmask within w_incluye_termografos
end type
type st_2 from statictext within w_incluye_termografos
end type
type st_1 from statictext within w_incluye_termografos
end type
type pb_salir from picturebutton within w_incluye_termografos
end type
end forward

global type w_incluye_termografos from window
integer width = 2117
integer height = 644
boolean titlebar = true
string title = "Números Termógrafo"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
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
em_termografo3 em_termografo3
em_termografo2 em_termografo2
st_5 st_5
st_4 st_4
em_termografo1 em_termografo1
st_2 st_2
st_1 st_1
pb_salir pb_salir
end type
global w_incluye_termografos w_incluye_termografos

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

SELECT COUNT(*)
	INTO :li_existe
	FROM dbo.termografo
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

on w_incluye_termografos.create
this.em_termografo3=create em_termografo3
this.em_termografo2=create em_termografo2
this.st_5=create st_5
this.st_4=create st_4
this.em_termografo1=create em_termografo1
this.st_2=create st_2
this.st_1=create st_1
this.pb_salir=create pb_salir
this.Control[]={this.em_termografo3,&
this.em_termografo2,&
this.st_5,&
this.st_4,&
this.em_termografo1,&
this.st_2,&
this.st_1,&
this.pb_salir}
end on

on w_incluye_termografos.destroy
destroy(this.em_termografo3)
destroy(this.em_termografo2)
destroy(this.st_5)
destroy(this.st_4)
destroy(this.em_termografo1)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.pb_salir)
end on

event open;x = 500
y = 1400

This.Icon	=	Gstr_apl.Icono

//istr_mant3.argumento[1] = dw_2.Object.defe_term01[row] 
//istr_mant3.argumento[2] = dw_2.Object.defe_term02[row] 
//istr_mant3.argumento[3] = dw_2.Object.defe_term03[row] 

em_termografo1.Text = istr_mant3.argumento[1]
em_termografo2.Text = istr_mant3.argumento[2]
em_termografo3.Text = istr_mant3.argumento[3]

//em_fechainspec.Text	   =	String(Today())
//istr_mant3.argumento[2] =	String(Today())


end event

type em_termografo3 from editmask within w_incluye_termografos
integer x = 603
integer y = 332
integer width = 1047
integer height = 100
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "aaaaaaaaaaaaaaa"
end type

event modified;IF istr_mant3.argumento[3] = '' THEN
	IF This.Text <> '' THEN
		IF existe_termografo(This.Text) THEN
			This.Text = ''
			This.SetFocus()
		ELSE
			IF em_termografo1.Text = This.Text OR em_termografo2.Text = This.Text THEN
				MessageBox("Atención","Termógrafo Ya se Incluyó ", Exclamation!, Ok!)
				This.Text = ''
				This.SetFocus()
				RETURN 1
			END IF	
			istr_mant3.argumento[3]	=	This.Text
		END IF	
	END IF	
ELSEIF istr_mant3.argumento[3] <> This.Text THEN
	IF This.Text <> '' THEN
		IF existe_termografo(This.Text) THEN
			This.Text = ''
			This.SetFocus()
		ELSE
			IF em_termografo1.Text = This.Text OR em_termografo2.Text = This.Text THEN
				MessageBox("Atención","Termógrafo Ya se Incluyó ", Exclamation!, Ok!)
				This.Text = ''
				This.SetFocus()
				RETURN 1
			END IF	
			istr_mant3.argumento[3]	=	This.Text
		END IF	
	ELSE
		istr_mant3.argumento[3]	 = ''
	END IF	
	
END IF	

end event

type em_termografo2 from editmask within w_incluye_termografos
integer x = 603
integer y = 204
integer width = 1047
integer height = 100
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "aaaaaaaaaaaaaaa"
end type

event modified;IF istr_mant3.argumento[2] = '' THEN
	IF This.Text <> '' THEN
		IF existe_termografo(This.Text) THEN
			This.Text = ''
			This.SetFocus()
		ELSE
			IF em_termografo1.Text = This.Text OR em_termografo3.Text = This.Text THEN
				MessageBox("Atención","Termógrafo Ya se Incluyó ", Exclamation!, Ok!)
				This.Text = ''
				This.SetFocus()
				RETURN 1
			END IF	
			istr_mant3.argumento[2]	=	This.Text
		END IF	
	END IF	
ELSEIF istr_mant3.argumento[2] <> This.Text THEN
	IF This.Text <> '' THEN
		IF existe_termografo(This.Text) THEN
			This.Text = ''
			This.SetFocus()
		ELSE
			IF em_termografo1.Text = This.Text OR em_termografo3.Text = This.Text THEN
				MessageBox("Atención","Termógrafo Ya se Incluyó ", Exclamation!, Ok!)
				This.Text = ''
				This.SetFocus()
				RETURN 1
			END IF
			istr_mant3.argumento[2]	=	This.Text
		END IF
	ELSE
		istr_mant3.argumento[2]	=	''
	END IF	
	
END IF	

end event

type st_5 from statictext within w_incluye_termografos
integer x = 82
integer y = 352
integer width = 402
integer height = 100
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Termógrafo 3"
boolean focusrectangle = false
end type

type st_4 from statictext within w_incluye_termografos
integer x = 82
integer y = 220
integer width = 402
integer height = 100
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Termógrafo 2"
boolean focusrectangle = false
end type

type em_termografo1 from editmask within w_incluye_termografos
integer x = 608
integer y = 76
integer width = 1047
integer height = 100
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "aaaaaaaaaaaaaaa"
end type

event modified;IF istr_mant3.argumento[1] = '' THEN
	IF This.Text <> '' THEN
		IF existe_termografo(This.Text) THEN
			This.Text = ''
			This.SetFocus()
		ELSE
			IF em_termografo2.Text = This.Text OR em_termografo3.Text = This.Text THEN
				MessageBox("Atención","Termógrafo Ya se Incluyó ", Exclamation!, Ok!)
				This.Text = ''
				This.SetFocus()
				RETURN 1
			END IF	
			istr_mant3.argumento[1]	=	This.Text
		END IF	
	END IF	
ELSEIF istr_mant3.argumento[1] <> This.Text THEN
	IF This.Text <> '' THEN
		IF existe_termografo(This.Text) THEN
			This.Text = ''
			This.SetFocus()
		ELSE
			IF em_termografo2.Text = This.Text OR em_termografo3.Text = This.Text THEN
				MessageBox("Atención","Termógrafo Ya se Incluyó ", Exclamation!, Ok!)
				This.Text = ''
				This.SetFocus()
				RETURN 1
			END IF	
			istr_mant3.argumento[1]	=	This.Text
		END IF	
	ELSE
		istr_mant3.argumento[1]	=	''
	END IF	
	
END IF	

end event

type st_2 from statictext within w_incluye_termografos
integer x = 82
integer y = 88
integer width = 402
integer height = 100
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Termógrafo 1"
boolean focusrectangle = false
end type

type st_1 from statictext within w_incluye_termografos
integer x = 32
integer y = 40
integer width = 1678
integer height = 432
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_incluye_termografos
integer x = 1774
integer y = 204
integer width = 302
integer height = 244
integer taborder = 40
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
alignment htextalign = left!
end type

event clicked;CloseWithReturn(Parent, istr_mant3)

end event

