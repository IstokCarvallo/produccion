$PBExportHeader$w_adhesivo_pallets.srw
forward
global type w_adhesivo_pallets from window
end type
type st_7 from statictext within w_adhesivo_pallets
end type
type st_6 from statictext within w_adhesivo_pallets
end type
type em_embalaje from editmask within w_adhesivo_pallets
end type
type em_calibre from editmask within w_adhesivo_pallets
end type
type st_5 from statictext within w_adhesivo_pallets
end type
type st_3 from statictext within w_adhesivo_pallets
end type
type uo_variedad from uo_seleccion_variedad within w_adhesivo_pallets
end type
type iuo_especie from uo_seleccion_especie within w_adhesivo_pallets
end type
type sle_pallet from editmask within w_adhesivo_pallets
end type
type st_2 from statictext within w_adhesivo_pallets
end type
type st_1 from statictext within w_adhesivo_pallets
end type
type st_4 from statictext within w_adhesivo_pallets
end type
type pb_nuevo from picturebutton within w_adhesivo_pallets
end type
type pb_salir from picturebutton within w_adhesivo_pallets
end type
type gb_3 from groupbox within w_adhesivo_pallets
end type
type cb_2 from commandbutton within w_adhesivo_pallets
end type
type cb_1 from commandbutton within w_adhesivo_pallets
end type
type iuo_cliente from uo_seleccion_cliente_comun within w_adhesivo_pallets
end type
type pb_imprimir from picturebutton within w_adhesivo_pallets
end type
type dw_2 from datawindow within w_adhesivo_pallets
end type
type cbx_visible from checkbox within w_adhesivo_pallets
end type
type gb_5 from groupbox within w_adhesivo_pallets
end type
type st_encabe from statictext within w_adhesivo_pallets
end type
end forward

global type w_adhesivo_pallets from window
integer width = 3899
integer height = 1184
boolean titlebar = true
string title = "EMISION DE ADHESIVOS PARA PALLETS"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 16777215
string icon = "AppIcon!"
event ue_nuevo ( )
event ue_recuperadatos ( )
event ue_imprimir ( )
st_7 st_7
st_6 st_6
em_embalaje em_embalaje
em_calibre em_calibre
st_5 st_5
st_3 st_3
uo_variedad uo_variedad
iuo_especie iuo_especie
sle_pallet sle_pallet
st_2 st_2
st_1 st_1
st_4 st_4
pb_nuevo pb_nuevo
pb_salir pb_salir
gb_3 gb_3
cb_2 cb_2
cb_1 cb_1
iuo_cliente iuo_cliente
pb_imprimir pb_imprimir
dw_2 dw_2
cbx_visible cbx_visible
gb_5 gb_5
st_encabe st_encabe
end type
global w_adhesivo_pallets w_adhesivo_pallets

type variables
Integer		ii_procedencia, ii_operacion, ii_sistema
Str_info		istr_info
uo_AnalizaPallet							iuo_pallet

end variables

forward prototypes
public function boolean existe_pallet (string as_pallet, string as_cliente)
public function boolean cliente_rotulado (integer as_cliente)
public function boolean existe_calibre (integer ai_especie, integer ai_variedad, string as_calibre)
public function boolean existe_embalaje (integer ai_cliente, string as_embalaje)
end prototypes

event ue_nuevo();iuo_cliente.seleccion(False, False)

iuo_cliente.Bloquear(False)

iuo_cliente.Codigo										=	gi_CodExport
cliente_rotulado(gi_CodExport)
iuo_cliente.dw_seleccion.Object.codigo[1]			=	iuo_cliente.Codigo

iuo_especie.seleccion(False, False)
//iuo_especie.Bloquear(False)
iuo_especie.Codigo										=	21
iuo_especie.Nombre										=  'Cerezas'
iuo_especie.dw_seleccion.Object.codigo[1]			=	iuo_especie.Codigo

uo_variedad.seleccion(False, False)
uo_variedad.Filtra(21)


sle_pallet.Text = ''
sle_pallet.SetFocus()






end event

event ue_imprimir();Integer	li_fila, li_codigo, li_pagina, li_nueva, li_copias, li_visible
String	ls_codigo, ls_paso, ls_pallet, ls_especie, ls_variedad
Date		ld_fecha

ls_paso = iuo_cliente.rotulado
ls_paso+= sle_pallet.Text

SetPointer(Arrow!)

//istr_info.titulo	= 'Impresión Datos Pallet'

//OpenWithParm(vinf, istr_info)

dw_2.DataObject = "dw_adhesivos_pallets"
dw_2.InsertRow(0)

//vinf.dw_1.SetTransObject(sqlca)
//vinf.dw_1.InsertRow(0)
dw_2.Object.pallet.Object.Text	=	ls_paso
dw_2.Object.t_pallet.Text			=	ls_paso

//vinf.dw_1.DataObject = "dw_adhesivos_pallets"

ls_Especie = String(iuo_especie.codigo)+' '+iuo_especie.nombre
ls_variedad = String(uo_variedad.codigo)+' '+uo_variedad.nombre

dw_2.Modify("t_especie.text = '" + ls_Especie + "'")
dw_2.Modify("t_variedad.text = '" + ls_variedad + "'")
dw_2.Modify("t_calibre.text = '" + em_calibre.Text + "'")
dw_2.Modify("t_embalaje.text = '" + em_embalaje.Text + "'")
		
//vinf.Visible	= True
//vinf.Enabled	= True
dw_2.Print()
dw_2.Reset()
sle_pallet.Text = ''
sle_pallet.SetFocus()
SetPointer(Arrow!)	
end event

public function boolean existe_pallet (string as_pallet, string as_cliente);Integer	ll_cont

SELECT	count(*)
	INTO	:ll_cont
	FROM	dbo.spro_palletencab
	WHERE	paen_numero	=	:as_pallet
	AND	clie_codigo =	:as_cliente ;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_palletencab")
	RETURN True
ELSEIF ll_cont > 0 THEN
	MessageBox("Atención", "Pallet Existe, digite otro.", &
					Exclamation!, Ok!)

	RETURN True
ELSE
	RETURN False
END IF

end function

public function boolean cliente_rotulado (integer as_cliente);String	ls_rotulado

SELECT	clie_rotula
	INTO	:ls_rotulado
	FROM	dbo.clientesprod
	WHERE	clie_codigo =	:as_cliente;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla clientesprod")
	RETURN True
ELSE
	iuo_cliente.Rotulado	=	String(ls_rotulado)
	RETURN False
END IF

end function

public function boolean existe_calibre (integer ai_especie, integer ai_variedad, string as_calibre);Integer	ll_cont

SELECT	count(*)
	INTO	:ll_cont
	FROM	dbo.variecalibre
	WHERE	espe_codigo	=	:ai_especie
	AND	vari_codigo =	:ai_variedad 
	AND	vaca_calibr =  :as_calibre;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla variecalibre")
	RETURN True
ELSEIF ll_cont = 0 THEN
	MessageBox("Atención", "Calibre NO Existe, digite otro.", &
					Exclamation!, Ok!)

	RETURN True
ELSE
	RETURN False
END IF

end function

public function boolean existe_embalaje (integer ai_cliente, string as_embalaje);Integer	ll_cont

SELECT	count(*)
	INTO	:ll_cont
	FROM	dbo.embalajesprod
	WHERE	clie_codigo	=	:ai_cliente
	AND	emba_codigo =	:as_embalaje;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla embalajesprod")
	RETURN True
ELSEIF ll_cont = 0 THEN
	MessageBox("Atención", "Embalaje NO Existe, digite otro.", &
					Exclamation!, Ok!)

	RETURN True
ELSE
	RETURN False
END IF

end function

on w_adhesivo_pallets.create
this.st_7=create st_7
this.st_6=create st_6
this.em_embalaje=create em_embalaje
this.em_calibre=create em_calibre
this.st_5=create st_5
this.st_3=create st_3
this.uo_variedad=create uo_variedad
this.iuo_especie=create iuo_especie
this.sle_pallet=create sle_pallet
this.st_2=create st_2
this.st_1=create st_1
this.st_4=create st_4
this.pb_nuevo=create pb_nuevo
this.pb_salir=create pb_salir
this.gb_3=create gb_3
this.cb_2=create cb_2
this.cb_1=create cb_1
this.iuo_cliente=create iuo_cliente
this.pb_imprimir=create pb_imprimir
this.dw_2=create dw_2
this.cbx_visible=create cbx_visible
this.gb_5=create gb_5
this.st_encabe=create st_encabe
this.Control[]={this.st_7,&
this.st_6,&
this.em_embalaje,&
this.em_calibre,&
this.st_5,&
this.st_3,&
this.uo_variedad,&
this.iuo_especie,&
this.sle_pallet,&
this.st_2,&
this.st_1,&
this.st_4,&
this.pb_nuevo,&
this.pb_salir,&
this.gb_3,&
this.cb_2,&
this.cb_1,&
this.iuo_cliente,&
this.pb_imprimir,&
this.dw_2,&
this.cbx_visible,&
this.gb_5,&
this.st_encabe}
end on

on w_adhesivo_pallets.destroy
destroy(this.st_7)
destroy(this.st_6)
destroy(this.em_embalaje)
destroy(this.em_calibre)
destroy(this.st_5)
destroy(this.st_3)
destroy(this.uo_variedad)
destroy(this.iuo_especie)
destroy(this.sle_pallet)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.st_4)
destroy(this.pb_nuevo)
destroy(this.pb_salir)
destroy(this.gb_3)
destroy(this.cb_2)
destroy(this.cb_1)
destroy(this.iuo_cliente)
destroy(this.pb_imprimir)
destroy(this.dw_2)
destroy(this.cbx_visible)
destroy(this.gb_5)
destroy(this.st_encabe)
end on

event open;TriggerEVent("ue_nuevo")
end event

type st_7 from statictext within w_adhesivo_pallets
integer x = 1861
integer y = 788
integer width = 288
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Embalaje"
boolean focusrectangle = false
end type

type st_6 from statictext within w_adhesivo_pallets
integer x = 169
integer y = 788
integer width = 503
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Calibre Rotulado"
boolean focusrectangle = false
end type

type em_embalaje from editmask within w_adhesivo_pallets
integer x = 2254
integer y = 764
integer width = 402
integer height = 112
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

event modified;IF Isnull(iuo_cliente.Codigo) OR iuo_cliente.Codigo = 0 THEN
	MessageBox("Atención", "Falta Cliente Para Continuar.", &
					Exclamation!, Ok!)
	Return					
END IF

IF NOT Isnull(This.Text) AND This.Text <> '' THEN
	IF existe_embalaje(iuo_cliente.Codigo,This.Text) THEN
		This.Text = ''
		This.SetFocus()
		Return
	END IF
END IF	
end event

type em_calibre from editmask within w_adhesivo_pallets
integer x = 731
integer y = 764
integer width = 402
integer height = 112
integer taborder = 50
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

event modified;IF Isnull(uo_variedad.Codigo) OR uo_variedad.Codigo = 0 THEN
	MessageBox("Atención", "Falta Variedad Para Continuar.", &
					Exclamation!, Ok!)
	Return					
END IF

IF Isnull(iuo_especie.Codigo) OR iuo_especie.Codigo = 0 THEN
	MessageBox("Atención", "Falta Especie Para Continuar.", &
					Exclamation!, Ok!)
	Return					
END IF

IF NOT Isnull(This.Text) AND This.Text <> '' THEN
	IF existe_calibre(iuo_especie.Codigo,uo_variedad.Codigo,This.Text) THEN
		em_calibre.Text = ''
		em_calibre.SetFocus()
		Return
	END IF
END IF	
end event

type st_5 from statictext within w_adhesivo_pallets
integer x = 1454
integer y = 584
integer width = 558
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad Rotulada"
boolean focusrectangle = false
end type

event clicked;IF NOT Isnull(iuo_cliente.Codigo) AND iuo_cliente.Codigo <> 0 THEN
	MessageBox("Atención", "Falta Cliente Para Continuar.", &
					Exclamation!, Ok!)
	Return					
END IF

IF NOT Isnull(This.Text) AND This.Text <> '' THEN
	IF existe_embalaje(iuo_cliente.Codigo,This.Text) THEN
		This.Text = ''
		Return
	END IF
END IF	
end event

type st_3 from statictext within w_adhesivo_pallets
integer x = 169
integer y = 584
integer width = 247
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type uo_variedad from uo_seleccion_variedad within w_adhesivo_pallets
integer x = 2062
integer y = 576
integer width = 887
integer height = 76
integer taborder = 40
end type

on uo_variedad.destroy
call uo_seleccion_variedad::destroy
end on

type iuo_especie from uo_seleccion_especie within w_adhesivo_pallets
integer x = 457
integer y = 580
integer width = 887
integer height = 84
integer taborder = 30
end type

on iuo_especie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;Integer	li_null

SetNull(li_null)
IF IsNull(This.Codigo) THEN 
	uo_variedad.dw_Seleccion.Enabled		=	True
	RETURN
END IF		

uo_variedad.Filtra(This.Codigo)
uo_variedad.dw_seleccion.Object.codigo[1]			=	li_null
uo_variedad.dw_Seleccion.Enabled		=	True



end event

type sle_pallet from editmask within w_adhesivo_pallets
integer x = 2226
integer y = 388
integer width = 430
integer height = 96
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "0000000"
end type

event modified;IF This.Text <> '0000000' THEN
	IF Existe_pallet(This.Text,string(iuo_cliente.codigo)) THEN
		This.Text = ''
		This.SetFocus()
	ELSE
		pb_imprimir.Enabled = True
	END IF
END IF	
end event

type st_2 from statictext within w_adhesivo_pallets
integer x = 41
integer y = 136
integer width = 3195
integer height = 124
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Impresión de Folio"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_1 from statictext within w_adhesivo_pallets
integer x = 1897
integer y = 420
integer width = 242
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Folio"
boolean focusrectangle = false
end type

type st_4 from statictext within w_adhesivo_pallets
integer x = 169
integer y = 420
integer width = 247
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type pb_nuevo from picturebutton within w_adhesivo_pallets
integer x = 3383
integer y = 248
integer width = 302
integer height = 244
integer taborder = 70
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Documento.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Documento-bn.png"
alignment htextalign = left!
end type

event clicked;em_calibre.Text = ''
em_embalaje.Text = ''

parent.triggerevent("ue_nuevo")

end event

type pb_salir from picturebutton within w_adhesivo_pallets
integer x = 3383
integer y = 776
integer width = 302
integer height = 244
integer taborder = 90
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png"
alignment htextalign = left!
end type

event clicked;Close(parent)
end event

type gb_3 from groupbox within w_adhesivo_pallets
boolean visible = false
integer x = 3127
integer y = 1608
integer width = 256
integer height = 248
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type cb_2 from commandbutton within w_adhesivo_pallets
boolean visible = false
integer x = 558
integer y = 564
integer width = 402
integer height = 100
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Ninguno"
end type

type cb_1 from commandbutton within w_adhesivo_pallets
boolean visible = false
integer x = 155
integer y = 564
integer width = 402
integer height = 100
integer taborder = 10
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "Todos"
end type

type iuo_cliente from uo_seleccion_cliente_comun within w_adhesivo_pallets
integer x = 457
integer y = 396
integer height = 88
integer taborder = 10
end type

on iuo_cliente.destroy
call uo_seleccion_cliente_comun::destroy
end on

type pb_imprimir from picturebutton within w_adhesivo_pallets
integer x = 3383
integer y = 516
integer width = 302
integer height = 244
integer taborder = 80
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Imprimir.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Imprimir-bn.png"
alignment htextalign = left!
end type

event clicked;IF Isnull(iuo_cliente.Codigo) OR iuo_cliente.Codigo = 0 THEN
	MessageBox("Atención", "Falta Cliente Para Continuar.", &
					Exclamation!, Ok!)
	Return					
END IF

IF Isnull(uo_variedad.Codigo) OR uo_variedad.Codigo = 0 THEN
	MessageBox("Atención", "Falta Variedad Para Continuar.", &
					Exclamation!, Ok!)
	Return					
END IF

IF Isnull(iuo_especie.Codigo) OR iuo_especie.Codigo = 0 THEN
	MessageBox("Atención", "Falta Especie Para Continuar.", &
					Exclamation!, Ok!)
	Return					
END IF

IF Isnull(em_calibre.Text) OR em_calibre.Text = '' THEN
	MessageBox("Atención", "Falta Calibre Para Continuar.", &
					Exclamation!, Ok!)
	Return					
END IF

IF Isnull(em_embalaje.Text) OR em_embalaje.Text = '' THEN
	MessageBox("Atención", "Falta Embalaje Para Continuar.", &
					Exclamation!, Ok!)
	Return					
END IF

Parent.TriggerEVent("ue_imprimir")
end event

type dw_2 from datawindow within w_adhesivo_pallets
boolean visible = false
integer x = 3305
integer y = 36
integer width = 229
integer height = 180
integer taborder = 110
string title = "none"
string dataobject = "dw_adhesivos_pallets"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type cbx_visible from checkbox within w_adhesivo_pallets
boolean visible = false
integer x = 55
integer y = 56
integer width = 672
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Formato Completo"
boolean checked = true
end type

event clicked;IF THIS.Checked THEN
	THIS.Text = 'Formato Completo'
	
ELSE
	THIS.Text = 'Solo Datos'
	
END IF
end event

type gb_5 from groupbox within w_adhesivo_pallets
boolean visible = false
integer x = 3127
integer y = 1152
integer width = 256
integer height = 320
integer taborder = 120
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type st_encabe from statictext within w_adhesivo_pallets
integer x = 73
integer y = 328
integer width = 3195
integer height = 692
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

