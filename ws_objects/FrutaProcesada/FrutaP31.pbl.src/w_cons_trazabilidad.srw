$PBExportHeader$w_cons_trazabilidad.srw
forward
global type w_cons_trazabilidad from window
end type
type st_4 from statictext within w_cons_trazabilidad
end type
type uo_selproductor from uo_seleccion_productor within w_cons_trazabilidad
end type
type dw_2 from uo_dw within w_cons_trazabilidad
end type
type pb_excel from picturebutton within w_cons_trazabilidad
end type
type cbx_cajas from checkbox within w_cons_trazabilidad
end type
type em_hasta from editmask within w_cons_trazabilidad
end type
type st_7 from statictext within w_cons_trazabilidad
end type
type cb_buscar from commandbutton within w_cons_trazabilidad
end type
type st_8 from statictext within w_cons_trazabilidad
end type
type em_numero from editmask within w_cons_trazabilidad
end type
type cbx_numero from checkbox within w_cons_trazabilidad
end type
type st_5 from statictext within w_cons_trazabilidad
end type
type em_desde from editmask within w_cons_trazabilidad
end type
type cbx_fecha from checkbox within w_cons_trazabilidad
end type
type uo_selespecie from uo_seleccion_especie within w_cons_trazabilidad
end type
type uo_selvariedad from uo_seleccion_variedad within w_cons_trazabilidad
end type
type st_3 from statictext within w_cons_trazabilidad
end type
type st_2 from statictext within w_cons_trazabilidad
end type
type st_1 from statictext within w_cons_trazabilidad
end type
type uo_selplanta from uo_seleccion_plantas within w_cons_trazabilidad
end type
type st_6 from statictext within w_cons_trazabilidad
end type
type uo_selcliente from uo_seleccion_clientesprod within w_cons_trazabilidad
end type
type pb_imprimir from picturebutton within w_cons_trazabilidad
end type
type pb_lectura from picturebutton within w_cons_trazabilidad
end type
type dw_1 from uo_dw within w_cons_trazabilidad
end type
type pb_salir from picturebutton within w_cons_trazabilidad
end type
type pb_nuevo from picturebutton within w_cons_trazabilidad
end type
type st_encabe from statictext within w_cons_trazabilidad
end type
end forward

global type w_cons_trazabilidad from window
integer x = 5
integer y = 16
integer width = 5120
integer height = 2076
boolean titlebar = true
string title = "Consulta Trazabilidad Movimientos Estacion"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
windowstate windowstate = maximized!
long backcolor = 16777215
event ue_asignacion ( )
st_4 st_4
uo_selproductor uo_selproductor
dw_2 dw_2
pb_excel pb_excel
cbx_cajas cbx_cajas
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
pb_imprimir pb_imprimir
pb_lectura pb_lectura
dw_1 dw_1
pb_salir pb_salir
pb_nuevo pb_nuevo
st_encabe st_encabe
end type
global w_cons_trazabilidad w_cons_trazabilidad

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
end variables

forward prototypes
public subroutine wf_cajasxpallet (long cliente, long planta, long pallet)
public subroutine wf_estacionesxpallet (long cliente, long planta, long pallet, integer estacion)
public subroutine wf_movtosxpallet (long cliente, long planta, long pallet)
end prototypes

event ue_asignacion();Integer				columna, li_fila
Str_busqueda		lstr_busq

il_fila	=	dw_1.GetRow()

dw_1.AcceptText()

lstr_busq.argum[1]	=	String(dw_1.Object.clie_codigo[il_fila])
lstr_busq.argum[2]	=	String(dw_1.Object.plde_codigo[il_fila])
lstr_busq.argum[3]	=	String(dw_1.Object.paen_numero[il_fila])


OpenWithParm(w_cons_movtosxpallet, lstr_busq)
end event

public subroutine wf_cajasxpallet (long cliente, long planta, long pallet);Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	String(Cliente)
lstr_busq.argum[2]	=	String(Planta)
lstr_busq.argum[3]	=	String(Pallet)

OpenWithParm(w_cons_cajasxpallet, lstr_busq)

Return
end subroutine

public subroutine wf_estacionesxpallet (long cliente, long planta, long pallet, integer estacion);Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	String(Cliente)
lstr_busq.argum[2]	=	String(Planta)
lstr_busq.argum[3]	=	String(Pallet)
lstr_busq.argum[4]	=	String(Estacion)

If Estacion = 4 Then
	OpenWithParm(w_cons_estacionesxpallet_pf, lstr_busq)
Else
	OpenWithParm(w_cons_estacionesxpallet, lstr_busq)
End If

Return
end subroutine

public subroutine wf_movtosxpallet (long cliente, long planta, long pallet);Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	String(Cliente)
lstr_busq.argum[2]	=	String(Planta)
lstr_busq.argum[3]	=	String(Pallet)

OpenWithParm(w_cons_movtosxpallet, lstr_busq)

Return
end subroutine

on w_cons_trazabilidad.create
this.st_4=create st_4
this.uo_selproductor=create uo_selproductor
this.dw_2=create dw_2
this.pb_excel=create pb_excel
this.cbx_cajas=create cbx_cajas
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
this.pb_imprimir=create pb_imprimir
this.pb_lectura=create pb_lectura
this.dw_1=create dw_1
this.pb_salir=create pb_salir
this.pb_nuevo=create pb_nuevo
this.st_encabe=create st_encabe
this.Control[]={this.st_4,&
this.uo_selproductor,&
this.dw_2,&
this.pb_excel,&
this.cbx_cajas,&
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
this.pb_imprimir,&
this.pb_lectura,&
this.dw_1,&
this.pb_salir,&
this.pb_nuevo,&
this.st_encabe}
end on

on w_cons_trazabilidad.destroy
destroy(this.st_4)
destroy(this.uo_selproductor)
destroy(this.dw_2)
destroy(this.pb_excel)
destroy(this.cbx_cajas)
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
destroy(this.pb_imprimir)
destroy(this.pb_lectura)
destroy(this.dw_1)
destroy(this.pb_salir)
destroy(this.pb_nuevo)
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

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(True, False)
	uo_SelEspecie.Seleccion(False, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	
	uo_SelEspecie.Codigo = gi_CodEspecie
	uo_SelEspecie.dw_Seleccion.Object.Codigo[1] = gi_CodEspecie
	uo_SelVariedad.Filtra(gi_CodEspecie)
	uo_SelCliente.Codigo = gi_CodExport
	uo_SelCliente.dw_Seleccion.Object.Codigo[1] = gi_CodExport
	
	em_Desde.Text	= String(RelativeDate(Today(), -30), 'dd/mm/yyyy hh:mm')
	em_Hasta.Text	= String(Today(), 'dd/mm/yyyy hh:mm')
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

IF st_encabe.Visible THEN
	li_posic_y				=	st_encabe.y
ELSE
	li_posic_y				=	dw_1.y
END IF

li_posic_x				=	This.WorkSpaceWidth() - 370

pb_lectura.x				=	li_posic_x
pb_lectura.y				=	li_posic_y
pb_lectura.width		=	li_Ancho
pb_lectura.height		=	li_Alto
li_posic_y 				+= li_Siguiente * 1.25

IF pb_nuevo.Visible THEN
	pb_nuevo.x			=	li_posic_x
	pb_nuevo.y			=	li_posic_y
	pb_nuevo.width	=	li_Ancho
	pb_nuevo.height	=	li_Alto
	li_visible++
	li_posic_y 			+= li_Siguiente
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			=	li_posic_x
	pb_imprimir.y			=	li_posic_y
	pb_imprimir.width		=	li_Ancho
	pb_imprimir.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_Excel.Visible THEN
	pb_Excel.x			=	li_posic_x
	pb_Excel.y			=	li_posic_y
	pb_Excel.width		=	li_Ancho
	pb_Excel.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

pb_salir.x				=	li_posic_x
pb_salir.y				=	dw_1.y + dw_1.Height - li_Siguiente
pb_salir.width			=	li_Ancho
pb_salir.height			=	li_Alto
end event

type st_4 from statictext within w_cons_trazabilidad
integer x = 1294
integer y = 176
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

type uo_selproductor from uo_seleccion_productor within w_cons_trazabilidad
integer x = 2523
integer y = 292
integer taborder = 50
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type dw_2 from uo_dw within w_cons_trazabilidad
boolean visible = false
integer x = 4699
integer y = 40
integer width = 302
integer height = 192
integer taborder = 40
boolean bringtotop = true
string dataobject = "dw_info_trazabilidad_excel"
end type

type pb_excel from picturebutton within w_cons_trazabilidad
integer x = 4750
integer y = 1068
integer width = 302
integer height = 244
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string picturename = "\Desarrollo 17\Imagenes\Botones\Excel.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Excel-bn.png"
alignment htextalign = left!
boolean map3dcolors = true
string powertiptext = "Genera Archivo Excel"
end type

event clicked;Long			ll_Fila, ll_Numero
String			ls_ruta, ls_archivo
Integer		li_Cajas= 0
DateTime	ld_Desde, ld_Hasta

If cbx_Fecha.Checked Then
	ld_Desde =	DateTime(Date('19000101'), Time('00:00'))
	ld_Hasta	=	DateTime(Today(), Now())
Else
	ld_Desde	=	DateTime(em_Desde.Text)
	ld_Hasta	=	DateTime(em_Hasta.Text)
End If

If cbx_Cajas.Checked Then li_Cajas = 1

If cbx_numero.Checked Then
	ll_Numero = -1
Else
	ll_Numero = Long(em_Numero.Text)
End If

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)

If GetFileSaveName ( "Seleccioanr archivo", ls_Ruta, ls_Archivo, "XLS", "XLS Files (*.xls),*.xls" , ls_Ruta, 32770) = -1 Then
	Messagebox('Error', "Al seleccionar archivo para grabar.", StopSign!, OK!)
	Return 
End If

dw_2.SetTransObject(Sqlca)
ll_Fila	=	dw_2.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, uo_SelEspecie.Codigo, uo_SelVariedad.Codigo, &
								ld_Desde, ld_Hasta, ll_Numero, li_Cajas, uo_SelProductor.Codigo)

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then 
	MessageBox( "No Existe información", "No existe información para generar este archivo.", StopSign!, Ok!)
Else
	If dw_2.SaveAs(ls_Ruta, Excel8!, True) = -1 Then
		MessageBox('Atencion', 'No se pudo generar archivo ' + ls_Archivo)
	Else
		MessageBox('Atencion', 'Se pudo generar archivo ' + ls_Archivo + '~n en ' + ls_Ruta )		
	End If
End If

end event

type cbx_cajas from checkbox within w_cons_trazabilidad
integer x = 3003
integer y = 60
integer width = 288
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
string text = "Cajas"
boolean lefttext = true
end type

type em_hasta from editmask within w_cons_trazabilidad
integer x = 3525
integer y = 164
integer width = 722
integer height = 92
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datetimemask!
string mask = "dd/mm/yyyy hh:mm"
boolean dropdowncalendar = true
end type

type st_7 from statictext within w_cons_trazabilidad
integer x = 3310
integer y = 172
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
boolean enabled = false
string text = "Hasta"
boolean focusrectangle = false
end type

type cb_buscar from commandbutton within w_cons_trazabilidad
integer x = 4210
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

type st_8 from statictext within w_cons_trazabilidad
integer x = 3502
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

type em_numero from editmask within w_cons_trazabilidad
integer x = 3698
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

type cbx_numero from checkbox within w_cons_trazabilidad
integer x = 3698
integer y = 272
integer width = 402
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

type st_5 from statictext within w_cons_trazabilidad
integer x = 3305
integer y = 64
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

type em_desde from editmask within w_cons_trazabilidad
integer x = 3525
integer y = 56
integer width = 722
integer height = 92
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datetimemask!
string mask = "dd/mm/yyyy hh:mm"
boolean dropdowncalendar = true
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

type cbx_fecha from checkbox within w_cons_trazabilidad
integer x = 4256
integer y = 64
integer width = 256
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
string text = "Todos"
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

type uo_selespecie from uo_seleccion_especie within w_cons_trazabilidad
event destroy ( )
integer x = 1573
integer y = 168
integer height = 84
integer taborder = 130
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

type uo_selvariedad from uo_seleccion_variedad within w_cons_trazabilidad
event destroy ( )
integer x = 1573
integer y = 292
integer taborder = 40
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type st_3 from statictext within w_cons_trazabilidad
integer x = 1289
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
boolean enabled = false
string text = "Variedad"
boolean focusrectangle = false
end type

type st_2 from statictext within w_cons_trazabilidad
integer x = 2533
integer y = 204
integer width = 873
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

type st_1 from statictext within w_cons_trazabilidad
integer x = 119
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
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type uo_selplanta from uo_seleccion_plantas within w_cons_trazabilidad
event destroy ( )
integer x = 352
integer y = 292
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_6 from statictext within w_cons_trazabilidad
integer x = 119
integer y = 176
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

type uo_selcliente from uo_seleccion_clientesprod within w_cons_trazabilidad
event destroy ( )
integer x = 352
integer y = 168
integer height = 84
integer taborder = 120
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type pb_imprimir from picturebutton within w_cons_trazabilidad
string tag = "Selección de Parámetros"
boolean visible = false
integer x = 4750
integer y = 764
integer width = 302
integer height = 244
integer taborder = 50
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Imprimir.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Imprimir-bn.png"
alignment htextalign = left!
end type

event clicked;Long			ll_Fila, ll_Numero
Integer		li_Cajas= 0
DateTime	ld_Desde, ld_Hasta

SetPointer(HourGlass!)

istr_info.titulo	=	"Consulta Trazabilidad"
istr_info.copias	=	1

If cbx_Fecha.Checked Then
	ld_Desde =	DateTime(Date('19000101'), Time('00:00'))
	ld_Hasta	=	DateTime(Today(), Now())
Else
	ld_Desde	=	DateTime(em_Desde.Text)
	ld_Hasta	=	DateTime(em_Hasta.Text)
End If

If cbx_Cajas.Checked Then li_Cajas = 1

If cbx_numero.Checked Then
	ll_Numero = -1
Else
	ll_Numero = Long(em_Numero.Text)
End If

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = ""
vinf.dw_1.SetTransObject(sqlca)
ll_Fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, uo_SelEspecie.Codigo, uo_SelVariedad.Codigo,&
							ld_Desde, ld_Hasta, ll_Numero, li_Cajas, uo_SelProductor.Codigo)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 THEN 
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_lectura from picturebutton within w_cons_trazabilidad
string tag = "Selección de Parámetros"
integer x = 4750
integer y = 524
integer width = 302
integer height = 244
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos-bn.png"
alignment htextalign = left!
end type

event clicked;Long			ll_Fila, ll_Numero
Integer		li_Cajas= 0
DateTime	ld_Desde, ld_Hasta

If cbx_Fecha.Checked Then
	ld_Desde =	DateTime(Date('19000101'), Time('00:00'))
	ld_Hasta	=	DateTime(Today(), Now())
Else
	ld_Desde	=	DateTime(em_Desde.Text)
	ld_Hasta	=	DateTime(em_Hasta.Text)
End If

If cbx_Cajas.Checked Then li_Cajas = 1

If cbx_numero.Checked Then
	ll_Numero = -1
Else
	ll_Numero = Long(em_Numero.Text)
End If

ll_Fila = dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, uo_SelEspecie.Codigo, uo_SelVariedad.Codigo,&
						ld_Desde, ld_Hasta, ll_Numero, 0, uo_SelProductor.Codigo)
	
IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox('ADVERTENCIA','No Existe información para este período', Information!, OK!)
	Return
End If
end event

type dw_1 from uo_dw within w_cons_trazabilidad
integer x = 37
integer y = 556
integer width = 4549
integer height = 1404
integer taborder = 100
boolean titlebar = true
string title = " Detalle de Movimiento Estaciones"
string dataobject = "dw_cons_trazabilidad"
boolean hscrollbar = true
end type

event rbuttondown;m_consultamovtos	l_Menu

If RowCount() =	0	Then
	Return
Else
	gstr_us.OpcionActiva	=	Parent.ClassName()
	il_fila 						=	Row
	This.SetRow(il_fila)
	
	l_Menu = Create m_consultamovtos
	
	m_consultamovtos.m_m_edicion.consultarecepcion.Visible		=	True
	l_Menu.m_m_edicion.PopMenu(This.PointerX(),This.PointerY()+750)
	
End If

end event

event clicked;call super::clicked;String		ls_Boton
Integer	li_Estacion

ls_Boton = dwo.Name

Choose Case ls_Boton
	Case 'b_cajas'
		wf_cajasxpallet(This.Object.clie_codigo[Row], This.Object.plde_codigo[Row], This.Object.paen_numero[Row])
		
	Case 'b_estacion01', 'b_estacion02', 'b_estacion03', 'b_estacion04', 'b_estacion05', 'b_estacion06',&
			'b_estacion07', 'b_estacion08', 'b_estacion09', 'b_estacion10', 'b_estacion11', 'b_estacion12'
			
			li_Estacion = Integer(Right(ls_Boton, 2))
		
			wf_estacionesxpallet(This.Object.clie_codigo[Row], This.Object.plde_codigo[Row], This.Object.paen_numero[Row], li_Estacion)
		
		
End Choose
end event

type pb_salir from picturebutton within w_cons_trazabilidad
integer x = 4750
integer y = 1552
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

type pb_nuevo from picturebutton within w_cons_trazabilidad
string tag = "Selección de Parámetros"
integer x = 4750
integer y = 1348
integer width = 302
integer height = 244
integer taborder = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string picturename = "\Desarrollo 17\Imagenes\Botones\Documento.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Documento-bn.png"
alignment htextalign = left!
end type

event clicked;dw_1.Reset()

end event

type st_encabe from statictext within w_cons_trazabilidad
integer x = 37
integer y = 48
integer width = 4549
integer height = 468
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

