$PBExportHeader$w_cons_codigooperacional.srw
forward
global type w_cons_codigooperacional from window
end type
type cb_codigo from commandbutton within w_cons_codigooperacional
end type
type em_operacional from editmask within w_cons_codigooperacional
end type
type st_10 from statictext within w_cons_codigooperacional
end type
type pb_grabar from picturebutton within w_cons_codigooperacional
end type
type cbx_calibre from checkbox within w_cons_codigooperacional
end type
type st_calidad from statictext within w_cons_codigooperacional
end type
type st_embalaje from statictext within w_cons_codigooperacional
end type
type cb_embalaje from commandbutton within w_cons_codigooperacional
end type
type cbx_embalaje from checkbox within w_cons_codigooperacional
end type
type uo_selpacking from uo_seleccion_plantas within w_cons_codigooperacional
end type
type st_9 from statictext within w_cons_codigooperacional
end type
type st_4 from statictext within w_cons_codigooperacional
end type
type uo_selproductor from uo_seleccion_productor within w_cons_codigooperacional
end type
type em_hasta from editmask within w_cons_codigooperacional
end type
type st_7 from statictext within w_cons_codigooperacional
end type
type cb_buscar from commandbutton within w_cons_codigooperacional
end type
type st_8 from statictext within w_cons_codigooperacional
end type
type em_numero from editmask within w_cons_codigooperacional
end type
type cbx_numero from checkbox within w_cons_codigooperacional
end type
type st_5 from statictext within w_cons_codigooperacional
end type
type em_desde from editmask within w_cons_codigooperacional
end type
type cbx_fecha from checkbox within w_cons_codigooperacional
end type
type uo_selespecie from uo_seleccion_especie within w_cons_codigooperacional
end type
type uo_selvariedad from uo_seleccion_variedad within w_cons_codigooperacional
end type
type st_3 from statictext within w_cons_codigooperacional
end type
type st_2 from statictext within w_cons_codigooperacional
end type
type st_1 from statictext within w_cons_codigooperacional
end type
type uo_selplanta from uo_seleccion_plantas within w_cons_codigooperacional
end type
type st_6 from statictext within w_cons_codigooperacional
end type
type uo_selcliente from uo_seleccion_clientesprod within w_cons_codigooperacional
end type
type pb_lectura from picturebutton within w_cons_codigooperacional
end type
type dw_1 from uo_dw within w_cons_codigooperacional
end type
type pb_salir from picturebutton within w_cons_codigooperacional
end type
type em_embalaje from singlelineedit within w_cons_codigooperacional
end type
type em_calibre from editmask within w_cons_codigooperacional
end type
type st_encabe from statictext within w_cons_codigooperacional
end type
end forward

global type w_cons_codigooperacional from window
integer x = 5
integer y = 16
integer width = 5102
integer height = 2308
boolean titlebar = true
string title = "Consulta Trazabilidad Movimientos Estacion"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
windowstate windowstate = maximized!
long backcolor = 16777215
event ue_asignacion ( )
cb_codigo cb_codigo
em_operacional em_operacional
st_10 st_10
pb_grabar pb_grabar
cbx_calibre cbx_calibre
st_calidad st_calidad
st_embalaje st_embalaje
cb_embalaje cb_embalaje
cbx_embalaje cbx_embalaje
uo_selpacking uo_selpacking
st_9 st_9
st_4 st_4
uo_selproductor uo_selproductor
em_hasta em_hasta
st_7 st_7
cb_buscar cb_buscar
st_8 st_8
em_numero em_numero
cbx_numero cbx_numero
st_5 st_5
em_desde em_desde
cbx_fecha cbx_fecha
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
st_3 st_3
st_2 st_2
st_1 st_1
uo_selplanta uo_selplanta
st_6 st_6
uo_selcliente uo_selcliente
pb_lectura pb_lectura
dw_1 dw_1
pb_salir pb_salir
em_embalaje em_embalaje
em_calibre em_calibre
st_encabe st_encabe
end type
global w_cons_codigooperacional w_cons_codigooperacional

type variables
String   ls_año_periodo
Long 		il_fila
Date		id_FechaAcceso
Time		it_HoraAcceso
Integer	ii_NroMes

Menu		im_menu

Str_parms		istr_parms
Str_mant			istr_mant
Str_busqueda	istr_busq
Str_info			istr_info


uo_calibre		iuo_calibre
end variables

on w_cons_codigooperacional.create
this.cb_codigo=create cb_codigo
this.em_operacional=create em_operacional
this.st_10=create st_10
this.pb_grabar=create pb_grabar
this.cbx_calibre=create cbx_calibre
this.st_calidad=create st_calidad
this.st_embalaje=create st_embalaje
this.cb_embalaje=create cb_embalaje
this.cbx_embalaje=create cbx_embalaje
this.uo_selpacking=create uo_selpacking
this.st_9=create st_9
this.st_4=create st_4
this.uo_selproductor=create uo_selproductor
this.em_hasta=create em_hasta
this.st_7=create st_7
this.cb_buscar=create cb_buscar
this.st_8=create st_8
this.em_numero=create em_numero
this.cbx_numero=create cbx_numero
this.st_5=create st_5
this.em_desde=create em_desde
this.cbx_fecha=create cbx_fecha
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.st_3=create st_3
this.st_2=create st_2
this.st_1=create st_1
this.uo_selplanta=create uo_selplanta
this.st_6=create st_6
this.uo_selcliente=create uo_selcliente
this.pb_lectura=create pb_lectura
this.dw_1=create dw_1
this.pb_salir=create pb_salir
this.em_embalaje=create em_embalaje
this.em_calibre=create em_calibre
this.st_encabe=create st_encabe
this.Control[]={this.cb_codigo,&
this.em_operacional,&
this.st_10,&
this.pb_grabar,&
this.cbx_calibre,&
this.st_calidad,&
this.st_embalaje,&
this.cb_embalaje,&
this.cbx_embalaje,&
this.uo_selpacking,&
this.st_9,&
this.st_4,&
this.uo_selproductor,&
this.em_hasta,&
this.st_7,&
this.cb_buscar,&
this.st_8,&
this.em_numero,&
this.cbx_numero,&
this.st_5,&
this.em_desde,&
this.cbx_fecha,&
this.uo_selespecie,&
this.uo_selvariedad,&
this.st_3,&
this.st_2,&
this.st_1,&
this.uo_selplanta,&
this.st_6,&
this.uo_selcliente,&
this.pb_lectura,&
this.dw_1,&
this.pb_salir,&
this.em_embalaje,&
this.em_calibre,&
this.st_encabe}
end on

on w_cons_codigooperacional.destroy
destroy(this.cb_codigo)
destroy(this.em_operacional)
destroy(this.st_10)
destroy(this.pb_grabar)
destroy(this.cbx_calibre)
destroy(this.st_calidad)
destroy(this.st_embalaje)
destroy(this.cb_embalaje)
destroy(this.cbx_embalaje)
destroy(this.uo_selpacking)
destroy(this.st_9)
destroy(this.st_4)
destroy(this.uo_selproductor)
destroy(this.em_hasta)
destroy(this.st_7)
destroy(this.cb_buscar)
destroy(this.st_8)
destroy(this.em_numero)
destroy(this.cbx_numero)
destroy(this.st_5)
destroy(this.em_desde)
destroy(this.cbx_fecha)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.uo_selplanta)
destroy(this.st_6)
destroy(this.uo_selcliente)
destroy(this.pb_lectura)
destroy(this.dw_1)
destroy(this.pb_salir)
destroy(this.em_embalaje)
destroy(this.em_calibre)
destroy(this.st_encabe)
end on

event open;Boolean	lb_Cerrar
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.Width									= dw_1.width + 540
This.Height									= 2500
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 180")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelVariedad.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelProductor.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPacking.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	iuo_Calibre =	Create uo_Calibre
	
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelEspecie.Seleccion(False, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	uo_SelPacking.Seleccion(True, False)
	
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gi_CodPlanta)
	uo_SelEspecie.Inicia(gi_CodEspecie)
	uo_SelVariedad.Filtra(gi_CodEspecie)
	uo_SelProductor.Filtra(-1)
	uo_SelPlanta.Filtra(1)
	uo_SelPacking.Filtra(2)
	
	em_Desde.Text	= String(RelativeDate(Today(), -30), 'dd/mm/yyyy')
	em_Hasta.Text	= String(Today(), 'dd/mm/yyyy')
End If
end event

event close;Boolean	Valida
Window	ventana
Integer	li_vta

ventana	= This.ParentWindow().GetFirstSheet()

IF IsValid(ventana) THEN
	li_vta++

	DO
		ventana	= This.ParentWindow().GetNextSheet(ventana)
		valida	= IsValid(ventana)
		IF valida THEN li_vta++
	LOOP WHILE valida
END IF

IF li_vta = 1 THEN
	This.ParentWindow().ToolBarVisible	= False
	im_menu.Item[1].Item[6].Enabled		= False
	im_menu.Item[7].Visible					= False
END IF

GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
end event

event resize;Integer		li_posic_x, li_posic_y, li_visible, &
				li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

dw_1.Resize(This.WorkSpaceWidth() - 490,This.WorkSpaceHeight() - dw_1.y - 75)

dw_1.x					=	78
st_encabe.width		=	dw_1.width

li_posic_y				=	dw_1.y
li_posic_x				=	This.WorkSpaceWidth() - 370

pb_lectura.x				=	li_posic_x
pb_lectura.y				=	li_posic_y
pb_lectura.width		=	li_Ancho
pb_lectura.height		=	li_Alto
li_posic_y 				+= li_Siguiente * 1.25

pb_grabar.x				=	li_posic_x
pb_grabar.y				=	li_posic_y
pb_grabar.width		=	li_Ancho
pb_grabar.height		=	li_Alto
li_posic_y 				+= li_Siguiente * 1.25

pb_salir.x				=	li_posic_x
pb_salir.y				=	dw_1.y + dw_1.Height - li_Siguiente
pb_salir.width			=	li_Ancho
pb_salir.height			=	li_Alto
end event

type cb_codigo from commandbutton within w_cons_codigooperacional
integer x = 1810
integer y = 492
integer width = 937
integer height = 112
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Codigo Operacional"
end type

event clicked;em_operacional.Text =  f_CodigoOPeracional(uo_SelEspecie.Codigo, em_operacional.Text)
end event

type em_operacional from editmask within w_cons_codigooperacional
integer x = 635
integer y = 496
integer width = 1138
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
textcase textcase = upper!
boolean displayonly = true
maskdatatype maskdatatype = stringmask!
string mask = "!!!!!!!!!!!!!!"
end type

event modified;If IsNull(This.Text) Then Return 

If NOT iuo_Calibre.Existe(uo_SelEspecie.Codigo, uo_SelVariedad.Codigo, This.Text, True, SQLCA) Then
	em_calibre.Text = ''
	This.SetFocus()
	Return 1
Else	
	em_calibre.Text	= iuo_calibre.calibre
	Return 1
End If	
end event

type st_10 from statictext within w_cons_codigooperacional
integer x = 87
integer y = 384
integer width = 238
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
string text = "Packing"
boolean focusrectangle = false
end type

type pb_grabar from picturebutton within w_cons_codigooperacional
string tag = "Grabar"
integer x = 4645
integer y = 964
integer width = 302
integer height = 244
integer taborder = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
alignment htextalign = left!
end type

event clicked;Long		ll_Fila, Folio, Planta, Productor
Integer	Especie, Variedad, Cliente, Etiqueta
String		Embalaje, Calibre, Codigo

If IsNull(em_operacional.Text) Then Return

SetPointer(HourGlass!)
dw_1.SetRedraw(False)

Codigo	= em_Operacional.Text

For ll_Fila = 1 To dw_1.RowCount()
	If dw_1.IsSelected(ll_Fila) Then
		//Script para carga de codigo operacional
		Cliente		= dw_1.Object.clie_codigo[ll_Fila]
		Planta			= dw_1.Object.plde_codigo[ll_Fila]
		Folio			= dw_1.Object.paen_numero[ll_Fila]
		Productor	= dw_1.Object.prod_codigo[ll_Fila]
		Especie		= dw_1.Object.espe_codigo[ll_Fila]
		Variedad		= dw_1.Object.vari_codigo[ll_Fila]
		Etiqueta		= dw_1.Object.etiq_codigo[ll_Fila]
		Embalaje		= dw_1.Object.emba_codigo[ll_Fila]
		Calibre		= dw_1.Object.pafr_calibr[ll_Fila]
		
		Update dbo.palletfruta
			Set pafr_codope = :Codigo
			Where clie_codigo = :Cliente
			And	plde_codigo = :Planta
			And	paen_numero = :Folio
			And 	prod_codigo = :Productor
			And	espe_codigo = :Especie
			And	vari_codigo = :Variedad
			And	etiq_codigo = :Etiqueta
			And	pafr_calibr = :Calibre
			And	emba_codigo = :Embalaje
			Using SQLCA;
		
		If SQLCA.SQLCode = -1 Then
			F_ErrorBaseDatos(SQLCA, "Actualizacion de Codigo Operacional")
		ElseIf SQLCA.SQLCode = 100 Then
		Else
			If SQLCA.SQLnRows = 0 Then
				
			End If	
		End If		
	End If
Next

Parent.pb_Lectura.TriggerEvent("clicked")

SetPointer(Arrow!)

dw_1.SetRedraw(True)

MessageBox('Atencion', 'Proceso de carga Codigo Operacional completado.', Information!, OK!)
end event

type cbx_calibre from checkbox within w_cons_codigooperacional
integer x = 3136
integer y = 196
integer width = 297
integer height = 80
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
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_calibre.Text			=	''
	em_calibre.Enabled		=	False
ELSE
	em_calibre.Enabled		=	True
	em_calibre.SetFocus()
END IF

end event

type st_calidad from statictext within w_cons_codigooperacional
integer x = 2427
integer y = 200
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
string text = "Calibre"
boolean focusrectangle = false
end type

type st_embalaje from statictext within w_cons_codigooperacional
integer x = 2427
integer y = 104
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
string text = "Embalaje"
boolean focusrectangle = false
end type

type cb_embalaje from commandbutton within w_cons_codigooperacional
integer x = 3045
integer y = 104
integer width = 96
integer height = 76
integer taborder = 190
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

lstr_busq.argum[1]	=	String(uo_SelCliente.Codigo)

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

If lstr_busq.argum[2] = "" Then
	em_embalaje.SetFocus()
Else
	If em_embalaje.Text <> '' Then
		em_embalaje.Text			=	em_embalaje.Text+','+lstr_busq.argum[2]
	Else	
		em_embalaje.Text			=	lstr_busq.argum[2]
	End If
End If
end event

type cbx_embalaje from checkbox within w_cons_codigooperacional
integer x = 3150
integer y = 100
integer width = 265
integer height = 80
integer taborder = 180
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_Embalaje.Enabled	=	False
	cb_Embalaje.Enabled		=	False
	em_Embalaje.Text			=	''	
ELSE
	em_Embalaje.Enabled	=	True
	cb_Embalaje.Enabled		=	True
END IF
end event

type uo_selpacking from uo_seleccion_plantas within w_cons_codigooperacional
event destroy ( )
integer x = 320
integer y = 288
integer height = 188
integer taborder = 20
boolean bringtotop = true
end type

on uo_selpacking.destroy
call uo_seleccion_plantas::destroy
end on

type st_9 from statictext within w_cons_codigooperacional
integer x = 87
integer y = 200
integer width = 238
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_4 from statictext within w_cons_codigooperacional
integer x = 1234
integer y = 104
integer width = 302
integer height = 64
boolean bringtotop = true
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

type uo_selproductor from uo_seleccion_productor within w_cons_codigooperacional
integer x = 2743
integer y = 292
integer taborder = 40
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type em_hasta from editmask within w_cons_codigooperacional
integer x = 3840
integer y = 192
integer width = 434
integer height = 92
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_7 from statictext within w_cons_codigooperacional
integer x = 3648
integer y = 200
integer width = 206
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
string text = "Hasta"
boolean focusrectangle = false
end type

type cb_buscar from commandbutton within w_cons_codigooperacional
integer x = 4338
integer y = 360
integer width = 119
integer height = 100
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
string text = "..."
end type

event clicked;str_busqueda	lstr_Busq

lstr_busq.argum[1]	=	String(uo_SelCliente.Codigo)
lstr_busq.argum[5]   = ''

OpenWithParm(w_busc_palletencab, lstr_busq)

lstr_busq = Message.PowerObjectParm

if UpperBound(lstr_busq.Argum) < 2 Then Return
If lstr_busq.Argum[2] <> "" Then	em_numero.Text 			=	lstr_busq.argum[2]

end event

type st_8 from statictext within w_cons_codigooperacional
integer x = 3657
integer y = 376
integer width = 210
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
string text = "Pallet"
boolean focusrectangle = false
end type

type em_numero from editmask within w_cons_codigooperacional
integer x = 3840
integer y = 352
integer width = 494
integer height = 112
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type cbx_numero from checkbox within w_cons_codigooperacional
integer x = 3840
integer y = 284
integer width = 402
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;If This.Checked Then
	cb_buscar.Enabled = False
	em_numero.Enabled = False
	em_numero.Text = ''
Else
	cb_buscar.Enabled = True
	em_numero.Enabled = True
	em_numero.Text = ''
End If
end event

type st_5 from statictext within w_cons_codigooperacional
integer x = 3648
integer y = 108
integer width = 206
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Desde"
boolean focusrectangle = false
end type

type em_desde from editmask within w_cons_codigooperacional
integer x = 3840
integer y = 96
integer width = 434
integer height = 92
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;//Date ld_Desde, ld_Hasta
//
//ld_Desde = Date(This.Text)
//ld_Hasta = Date(em_Hasta.Text)
//
//If ( ld_Desde > ld_Hasta)  Then
//	MessageBox('Alerta', "Fecha no puede ser mayor a Fecha " + em_Hasta.Text, Information!, OK!)
//	This.Text = ''
//End If
end event

type cbx_fecha from checkbox within w_cons_codigooperacional
integer x = 4279
integer y = 100
integer width = 165
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
string text = "T"
end type

event clicked;If This.Checked Then
	em_Desde.Enabled = False
	em_Hasta.Enabled = False
	em_Desde.Text = ''
	em_Hasta.Text = ''
Else
	em_Desde.Enabled = True
	em_Hasta.Enabled = True
	em_Desde.Text	= String(RelativeDate(Today(), -30), 'dd/mm/yyyy hh:mm')
	em_Hasta.Text	= String(Today(), 'dd/mm/yyyy hh:mm')
End If
end event

type uo_selespecie from uo_seleccion_especie within w_cons_codigooperacional
event destroy ( )
integer x = 1513
integer y = 96
integer height = 84
integer taborder = 160
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelVariedad.Filtra(-1)
		
	Case Else
		uo_SelVariedad.Filtra(This.Codigo)
		
End Choose
	
end event

type uo_selvariedad from uo_seleccion_variedad within w_cons_codigooperacional
event destroy ( )
integer x = 1513
integer y = 292
integer taborder = 30
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type st_3 from statictext within w_cons_codigooperacional
integer x = 1230
integer y = 384
integer width = 302
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
string text = "Variedad"
boolean focusrectangle = false
end type

type st_2 from statictext within w_cons_codigooperacional
integer x = 2427
integer y = 388
integer width = 315
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

type st_1 from statictext within w_cons_codigooperacional
integer x = 87
integer y = 512
integer width = 507
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
string text = "Cod. Operacional"
boolean focusrectangle = false
end type

type uo_selplanta from uo_seleccion_plantas within w_cons_codigooperacional
event destroy ( )
integer x = 320
integer y = 196
integer height = 84
integer taborder = 10
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_6 from statictext within w_cons_codigooperacional
integer x = 87
integer y = 104
integer width = 238
integer height = 64
boolean bringtotop = true
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

type uo_selcliente from uo_seleccion_clientesprod within w_cons_codigooperacional
event destroy ( )
integer x = 320
integer y = 96
integer height = 84
integer taborder = 150
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type pb_lectura from picturebutton within w_cons_codigooperacional
string tag = "Selección de Parámetros"
integer x = 4640
integer y = 696
integer width = 302
integer height = 244
integer taborder = 70
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos-bn.png"
alignment htextalign = left!
end type

event clicked;Long		ll_Fila, ll_Numero
Date		ld_Desde, ld_Hasta
String		ls_Embalaje, ls_Calibre			

If cbx_Fecha.Checked Then
	ld_Desde =	Date('19000101')
	ld_Hasta	=	Today()
Else
	ld_Desde	=	Date(em_Desde.Text)
	ld_Hasta	=	Date(em_Hasta.Text)
End If

If cbx_Embalaje.Checked Then
	ls_Embalaje = '*'
Else
	ls_Embalaje = em_Embalaje.Text
End If

If cbx_Calibre.Checked Then
	ls_Calibre = '*'
Else
	ls_Calibre = em_Calibre.Text
End If

If cbx_numero.Checked Then
	ll_Numero = -1
Else
	ll_Numero = Long(em_Numero.Text)
End If

ll_Fila = dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, uo_SelPacking.Codigo, uo_SelProductor.Codigo, &
						uo_SelEspecie.Codigo, uo_SelVariedad.Codigo, ls_Embalaje, ls_Calibre, ld_Desde, ld_Hasta, ll_Numero)
	
IF ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox('ADVERTENCIA','No Existe información para este período', Information!, OK!)
	Return
Else
	em_operacional.Enabled	=	True
	pb_Grabar.Enabled 		= True
End If
end event

type dw_1 from uo_dw within w_cons_codigooperacional
integer x = 37
integer y = 652
integer width = 4549
integer height = 1488
integer taborder = 110
string title = " Detalle de Movimiento Estaciones"
string dataobject = "dw_cons_codigooperacional"
boolean hscrollbar = true
end type

event clicked;call super::clicked;Long	ll_Fila, ll_SelectedRow

If Row = 0 Then
	/*
	Para que funcione este ordenamiento los títulos deben tener el nombre
	de la columna y terminacion "_t", de lo contrario no funcionará
	*/
	String		ls_old_sort, ls_column, ls_color_old
	Char		lc_sort
	
	If IsNull(dwo) Then RETURN
	
	If Right(dwo.Name,2) = '_t' Then
		ls_column	= Left (dwo.Name, Len(String(dwo.Name)) - 2)
		ls_old_sort	= This.Describe("Datawindow.Table.sort")
		ls_color_old	=This.Describe(ls_Column + "_t.Color")
	
		If ls_column = Left(ls_old_sort, Len(ls_old_sort) - 2) Then
			lc_sort = Right(ls_old_sort, 1)
			If lc_sort = 'A' Then
				lc_sort = 'D'
			Else
				lc_sort = 'A'
			End If
			This.SetSort(ls_column+" "+lc_sort)
		Else
			This.SetSort(ls_column+" A")
			This.ModIfy(Left(ls_old_sort, Len(ls_old_sort) - 2) + "_t.Color = " + ls_color_old)
		End If
		
		This.ModIfy(dwo.Name + ".Color = " + String(Rgb(0, 0, 255)))
		
		This.Sort()
	End If

Else
	ll_SelectedRow = This.GetSelectedRow(0)
	
	If KeyDown(keyShIft!) Then
		If ll_SelectedRow = 0 Then
			This.SelectRow(Row, True)
		Else
			This.SelectRow(1, False)
			If Row > ll_SelectedRow Then
				For ll_Fila = ll_SelectedRow To Row
					This.SelectRow(ll_Fila, True)
				Next
			Else
				For ll_Fila = Row To ll_SelectedRow
					This.SelectRow(ll_Fila, True)
				Next
			End If
		End If
	
	ElseIf KeyDown(keyControl!) Then
		If This.IsSelected(Row) Then
			This.SelectRow(Row, False)
		Else
			This.SelectRow(Row, True)
		End If
	
	Else
		If This.IsSelected(Row) Then
			This.SelectRow(0, False)
			This.SelectRow(Row, True)
		Else
			This.SelectRow(0, False)
			This.SelectRow(Row, True)
		End If
	End If
End If
end event

type pb_salir from picturebutton within w_cons_codigooperacional
integer x = 4649
integer y = 1608
integer width = 302
integer height = 244
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png"
alignment htextalign = left!
end type

event pb_salir::clicked;call super::clicked;Close(parent)
end event

type em_embalaje from singlelineedit within w_cons_codigooperacional
integer x = 2743
integer y = 100
integer width = 297
integer height = 84
integer taborder = 120
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
end type

event modified;String	ls_embalaje, ls_Nombre

ls_embalaje	=	This.Text

SELECT	emba_nombre
	INTO	:ls_Nombre
	FROM	dbo.embalajesprod
	WHERE	clie_codigo	=	:uo_SelCliente.Codigo
	AND	emba_codigo	=	:ls_embalaje;

If ls_Nombre <> '' Then
	istr_mant.argumento[8]	=	ls_embalaje
Else
	istr_mant.argumento[8]	=	ls_embalaje
End If	

end event

type em_calibre from editmask within w_cons_codigooperacional
integer x = 2743
integer y = 196
integer width = 297
integer height = 84
integer taborder = 130
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxxxx"
end type

event modified;If IsNull(This.Text) Then Return 

If NOT iuo_Calibre.Existe(uo_SelEspecie.Codigo, uo_SelVariedad.Codigo, This.Text, True, SQLCA) Then
	em_calibre.Text = ''
	This.SetFocus()
	Return 1
Else	
	em_calibre.Text	= iuo_calibre.calibre
	Return 1
End If	
end event

type st_encabe from statictext within w_cons_codigooperacional
integer x = 37
integer y = 48
integer width = 4549
integer height = 576
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

