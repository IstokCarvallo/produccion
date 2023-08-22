$PBExportHeader$w_caracteristicas_condicion.srw
forward
global type w_caracteristicas_condicion from w_para_informes
end type
type dw_2 from datawindow within w_caracteristicas_condicion
end type
type dw_3 from datawindow within w_caracteristicas_condicion
end type
type dw_4 from datawindow within w_caracteristicas_condicion
end type
type st_calidad from statictext within w_caracteristicas_condicion
end type
type em_calidad from editmask within w_caracteristicas_condicion
end type
type cbx_calidad from checkbox within w_caracteristicas_condicion
end type
type st_embalaje from statictext within w_caracteristicas_condicion
end type
type em_embalaje from editmask within w_caracteristicas_condicion
end type
type cb_buscaembalaje from commandbutton within w_caracteristicas_condicion
end type
type cbx_embalaje from checkbox within w_caracteristicas_condicion
end type
type st_1 from statictext within w_caracteristicas_condicion
end type
type st_2 from statictext within w_caracteristicas_condicion
end type
type st_3 from statictext within w_caracteristicas_condicion
end type
type cbx_1 from checkbox within w_caracteristicas_condicion
end type
type cbx_2 from checkbox within w_caracteristicas_condicion
end type
type cbx_3 from checkbox within w_caracteristicas_condicion
end type
type em_inspeccion from editmask within w_caracteristicas_condicion
end type
type cbx_4 from checkbox within w_caracteristicas_condicion
end type
type st_4 from statictext within w_caracteristicas_condicion
end type
end forward

global type w_caracteristicas_condicion from w_para_informes
integer x = 521
integer y = 656
integer width = 2235
integer height = 1480
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
dw_2 dw_2
dw_3 dw_3
dw_4 dw_4
st_calidad st_calidad
em_calidad em_calidad
cbx_calidad cbx_calidad
st_embalaje st_embalaje
em_embalaje em_embalaje
cb_buscaembalaje cb_buscaembalaje
cbx_embalaje cbx_embalaje
st_1 st_1
st_2 st_2
st_3 st_3
cbx_1 cbx_1
cbx_2 cbx_2
cbx_3 cbx_3
em_inspeccion em_inspeccion
cbx_4 cbx_4
st_4 st_4
end type
global w_caracteristicas_condicion w_caracteristicas_condicion

type variables
Str_mant	istr_mant

Integer	ii_planta

DataWindowchild idwc_mercado, idwc_especie, idwc_variedad, idwc_categoria

uo_calibre					iuo_calibre
end variables

on w_caracteristicas_condicion.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.dw_3=create dw_3
this.dw_4=create dw_4
this.st_calidad=create st_calidad
this.em_calidad=create em_calidad
this.cbx_calidad=create cbx_calidad
this.st_embalaje=create st_embalaje
this.em_embalaje=create em_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.cbx_embalaje=create cbx_embalaje
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.cbx_1=create cbx_1
this.cbx_2=create cbx_2
this.cbx_3=create cbx_3
this.em_inspeccion=create em_inspeccion
this.cbx_4=create cbx_4
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.dw_3
this.Control[iCurrent+3]=this.dw_4
this.Control[iCurrent+4]=this.st_calidad
this.Control[iCurrent+5]=this.em_calidad
this.Control[iCurrent+6]=this.cbx_calidad
this.Control[iCurrent+7]=this.st_embalaje
this.Control[iCurrent+8]=this.em_embalaje
this.Control[iCurrent+9]=this.cb_buscaembalaje
this.Control[iCurrent+10]=this.cbx_embalaje
this.Control[iCurrent+11]=this.st_1
this.Control[iCurrent+12]=this.st_2
this.Control[iCurrent+13]=this.st_3
this.Control[iCurrent+14]=this.cbx_1
this.Control[iCurrent+15]=this.cbx_2
this.Control[iCurrent+16]=this.cbx_3
this.Control[iCurrent+17]=this.em_inspeccion
this.Control[iCurrent+18]=this.cbx_4
this.Control[iCurrent+19]=this.st_4
end on

on w_caracteristicas_condicion.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.st_calidad)
destroy(this.em_calidad)
destroy(this.cbx_calidad)
destroy(this.st_embalaje)
destroy(this.em_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.cbx_embalaje)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.cbx_1)
destroy(this.cbx_2)
destroy(this.cbx_3)
destroy(this.em_inspeccion)
destroy(this.cbx_4)
destroy(this.st_4)
end on

event open;call super::open;istr_mant = Message.PowerObjectParm

dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(SQLCA)
idwc_especie.Retrieve()
dw_2.InsertRow(0)
dw_2.SetItem(1, "espe_codigo", 11)

istr_mant.argumento[11] = '11'

dw_3.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(SQLCA)
idwc_variedad.Retrieve(11)
dw_3.InsertRow(0)
dw_3.SetItem(1, "vari_codigo", 1)
istr_mant.Argumento[12] = '1'

dw_4.GetChild("cate_codigo", idwc_categoria)
idwc_categoria.SetTransObject(SQLCA)
idwc_categoria.Retrieve(-1)
dw_4.InsertRow(0)
dw_4.SetItem(1, "cate_codigo", 1)
istr_mant.Argumento[13] = '1'


istr_mant.argumento[16] = '1'

iuo_calibre   						=	Create uo_calibre



end event

type pb_excel from w_para_informes`pb_excel within w_caracteristicas_condicion
end type

type st_computador from w_para_informes`st_computador within w_caracteristicas_condicion
end type

type st_usuario from w_para_informes`st_usuario within w_caracteristicas_condicion
end type

type st_temporada from w_para_informes`st_temporada within w_caracteristicas_condicion
end type

type p_logo from w_para_informes`p_logo within w_caracteristicas_condicion
end type

type st_titulo from w_para_informes`st_titulo within w_caracteristicas_condicion
integer width = 1467
string text = "Seleccion Característica"
end type

type pb_acepta from w_para_informes`pb_acepta within w_caracteristicas_condicion
string tag = "Imprimir Reporte"
integer x = 1879
integer y = 572
integer taborder = 30
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
end type

event pb_acepta::clicked;Integer	li_cont, li_cliente, li_planta
String	ls_mensaje, ls_colu[]
Long		ll_cont,ll_numero

li_cliente = Integer(istr_mant.argumento[3])
li_planta  = Integer(istr_mant.argumento[1])


IF cbx_4.Checked THEN

	//ll_numero  = long(istr_mant.argumento[18])
	
	IF em_inspeccion.Text ="" THEN
		MessageBox("Atención", "El Número de Inspección fue Ingresado.", &
		Exclamation!, OK!)
		Return
	ELSE 
		ll_numero  = long(istr_mant.argumento[18])
	END IF
	//modificacion el valor si llega null
	//	IF ll_numero = '' THEN
	//		MessageBox("Atención", "Número de Inspección NO existe.", &
	//			Exclamation!, OK!)
	//		RETURN 
	//	END IF
	//MessageBox( 'Message', 'Total events so far' + string(ll_numero))
	
	SELECT count(*) 
	INTO :ll_cont
	FROM dbo.inspecpaldet
	WHERE clie_codigo = :li_cliente 
	AND plde_codigo = :li_planta
	AND inpe_numero = :ll_numero;
		
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla inspecpaldet")
		RETURN 
	ELSEIF ll_cont = 0 THEN
		MessageBox("Atención", "Número de Inspección NO existe.", &
			Exclamation!, OK!)
		Return
	END IF
ELSE
	IF Isnull(dw_2.Object.espe_codigo[1]) OR dw_2.Object.espe_codigo[1] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo de especie"
		ls_colu[li_cont]	= "espe_codigo"
	END IF
	
	IF Isnull(dw_3.Object.vari_codigo[1]) OR dw_3.Object.vari_codigo[1] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo de Variedad"
		ls_colu[li_cont]	= "vari_codigo"
	END IF
	
	IF Isnull(dw_4.Object.cate_codigo[1]) OR dw_4.Object.cate_codigo[1] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo de Categoria"
		ls_colu[li_cont]	= "cat_codigo"
	END IF
	
	IF em_calidad.Text = "" THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nCalibre del Pallet"
		ls_colu[li_cont]	= "pafr_calibr"
	END IF
	
	IF em_embalaje.Text ="" THEN
		li_cont ++
		ls_mensaje			= ls_mensaje + "~nEmbalaje"
		ls_colu[li_cont]	= "emba_codigo"
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
		dw_2.SetFocus()
		Return
	END IF
END IF	
istr_mant.argumento[17] = '1'

CloseWithReturn(Parent,istr_mant)




end event

type pb_salir from w_para_informes`pb_salir within w_caracteristicas_condicion
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1879
integer y = 936
integer taborder = 40
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

event pb_salir::clicked;istr_mant.argumento[17]	= '-1'


CloseWithReturn(Parent,istr_mant)
end event

type dw_2 from datawindow within w_caracteristicas_condicion
integer x = 672
integer y = 672
integer width = 1006
integer height = 120
integer taborder = 50
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[11] = data


dw_3.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(SQLCA)
idwc_variedad.Retrieve(Integer(data))
dw_3.InsertRow(0)
end event

type dw_3 from datawindow within w_caracteristicas_condicion
integer x = 672
integer y = 788
integer width = 1006
integer height = 120
integer taborder = 60
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_variedades"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[12] = data
end event

type dw_4 from datawindow within w_caracteristicas_condicion
integer x = 672
integer y = 904
integer width = 1006
integer height = 120
integer taborder = 70
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_categorias"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[13] = data
end event

type st_calidad from statictext within w_caracteristicas_condicion
integer x = 329
integer y = 684
integer width = 265
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Especies"
boolean focusrectangle = false
end type

type em_calidad from editmask within w_caracteristicas_condicion
integer x = 672
integer y = 1024
integer width = 297
integer height = 84
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxx"
end type

event modified;Integer	li_especie, li_variedad, li_cantid
String	ls_calibre

li_especie	=	Integer(istr_mant.argumento[11]) // Especie
li_variedad	=	Integer(istr_mant.argumento[12]) // Variedad
ls_calibre	=	This.Text

IF NOT iuo_calibre.existe(li_especie,li_variedad,ls_calibre,True,sqlca) THEN
	This.Text = ''
	//This.SetFocus()
	Return
ELSE
	ls_Calibre = iuo_calibre.calibre
	istr_mant.argumento[14]	=	ls_calibre
	em_calidad.Text			=	ls_calibre
	Return
END IF	

end event

type cbx_calidad from checkbox within w_caracteristicas_condicion
boolean visible = false
integer x = 1033
integer y = 1856
integer width = 297
integer height = 80
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todas"
end type

event clicked;IF This.Checked THEN
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[7]	=	'*'
ELSE
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF

end event

type st_embalaje from statictext within w_caracteristicas_condicion
integer x = 329
integer y = 804
integer width = 288
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Variedad"
boolean focusrectangle = false
end type

type em_embalaje from editmask within w_caracteristicas_condicion
integer x = 672
integer y = 1140
integer width = 297
integer height = 84
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxxxxxxxxx"
end type

event modified;Integer  li_cliente
String	ls_embalaje, ls_Nombre

li_cliente	=	Integer(istr_mant.argumento[3]) // Cliente
ls_embalaje	=	This.Text

SELECT	emba_nombre
	INTO	:ls_Nombre
	FROM	dbo.embalajesprod
	WHERE	clie_codigo	=	:li_cliente
	AND	emba_codigo	=	:ls_embalaje;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Embalajes")
	This.SetFocus()
ELSEIF (ls_Nombre = '' OR isnull(ls_Nombre))  THEN
	MessageBox("Atención", "Código de Embalaje no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	em_embalaje.Text = ''	
	This.SetFocus()
ELSE
	istr_mant.argumento[15]	=	ls_embalaje

END IF
end event

type cb_buscaembalaje from commandbutton within w_caracteristicas_condicion
integer x = 983
integer y = 1140
integer width = 96
integer height = 76
integer taborder = 180
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "..."
end type

event clicked;Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	istr_mant.argumento[3] // Cliente

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	em_embalaje.SetFocus()
ELSE
	em_embalaje.Text			=	lstr_busq.argum[2]
	istr_mant.argumento[15]	=	lstr_busq.argum[2]

END IF
end event

type cbx_embalaje from checkbox within w_caracteristicas_condicion
boolean visible = false
integer x = 1033
integer y = 2012
integer width = 283
integer height = 80
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todos"
end type

event clicked;IF This.Checked THEN
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
	istr_mant.argumento[8]		=	'Z'
 
	
ELSE
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
	

END IF
end event

type st_1 from statictext within w_caracteristicas_condicion
integer x = 329
integer y = 920
integer width = 297
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Categoria"
boolean focusrectangle = false
end type

type st_2 from statictext within w_caracteristicas_condicion
integer x = 329
integer y = 1028
integer width = 288
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Calidad"
boolean focusrectangle = false
end type

type st_3 from statictext within w_caracteristicas_condicion
integer x = 329
integer y = 1140
integer width = 288
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Embalaje"
boolean focusrectangle = false
end type

type cbx_1 from checkbox within w_caracteristicas_condicion
boolean visible = false
integer x = 1029
integer y = 1416
integer width = 297
integer height = 80
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todas"
end type

event clicked;IF This.Checked THEN
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[7]	=	'*'
ELSE
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF

end event

type cbx_2 from checkbox within w_caracteristicas_condicion
boolean visible = false
integer x = 1033
integer y = 1560
integer width = 297
integer height = 80
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todas"
end type

event clicked;IF This.Checked THEN
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[7]	=	'*'
ELSE
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF

end event

type cbx_3 from checkbox within w_caracteristicas_condicion
boolean visible = false
integer x = 1033
integer y = 1704
integer width = 297
integer height = 80
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Todas"
end type

event clicked;IF This.Checked THEN
	em_calidad.Text			=	''
	em_calidad.Enabled		=	False
	istr_mant.argumento[7]	=	'*'
ELSE
	em_calidad.Enabled		=	True
	em_calidad.SetFocus()
END IF

end event

type em_inspeccion from editmask within w_caracteristicas_condicion
integer x = 914
integer y = 476
integer width = 366
integer height = 84
integer taborder = 180
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxxxxxxxxx"
end type

event modified;istr_mant.argumento[18]	=	em_inspeccion.Text
end event

type cbx_4 from checkbox within w_caracteristicas_condicion
integer x = 306
integer y = 476
integer width = 512
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Por Inspección"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_inspeccion.Enabled = True
	dw_2.Enabled = False
	dw_3.Enabled = False
	dw_4.Enabled = False
	em_calidad.Enabled = False
	em_embalaje.Enabled = False
	cb_buscaembalaje.Enabled = False
	em_calidad.Text = ''
	em_embalaje.Text = ''
	istr_mant.argumento[16] = '1'
ELSE
	em_inspeccion.Enabled = False
	dw_2.Enabled = True
	dw_3.Enabled = True
	dw_4.Enabled = True
	em_calidad.Enabled = True
	em_embalaje.Enabled = True
	cb_buscaembalaje.Enabled = True
	em_inspeccion.Text = ''
	istr_mant.argumento[16] = '2'
END IF	
end event

type st_4 from statictext within w_caracteristicas_condicion
integer x = 251
integer y = 448
integer width = 1467
integer height = 848
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

