$PBExportHeader$w_ingresocamiones_systray.srw
forward
global type w_ingresocamiones_systray from w_systray
end type
type dw_ticket from datawindow within w_ingresocamiones_systray
end type
type st_mensaje from statictext within w_ingresocamiones_systray
end type
type st_estado from statictext within w_ingresocamiones_systray
end type
type pb_lectura from picturebutton within w_ingresocamiones_systray
end type
type pb_salir from picturebutton within w_ingresocamiones_systray
end type
type dw_2 from datawindow within w_ingresocamiones_systray
end type
type dw_1 from datawindow within w_ingresocamiones_systray
end type
type notifyicondata from structure within w_ingresocamiones_systray
end type
end forward

type notifyicondata from structure
	long		cbsize
	long		hwnd
	long		uid
	long		uflags
	long		ucallbackmessage
	long		hicon
	character		sztip[64]
end type

global type w_ingresocamiones_systray from w_systray
string tag = "Movimientos Camiones"
integer width = 3145
integer height = 2620
string title = "Movimientos Camiones"
windowstate windowstate = normal!
string icon = "AppIcon!"
event ue_antesguardar ( )
event ue_guardar ( )
event ue_nuevo ( )
event ue_recuperadatos ( )
dw_ticket dw_ticket
st_mensaje st_mensaje
st_estado st_estado
pb_lectura pb_lectura
pb_salir pb_salir
dw_2 dw_2
dw_1 dw_1
end type
global w_ingresocamiones_systray w_ingresocamiones_systray

type variables
uo_TipoCamion		iuo_Camion
uo_Transportista	iuo_Transportista

Date					id_fecha
Time					it_time

protected:
Long					il_fila
String					buscar, ordenar, is_ultimacol, ias_campo[]
Boolean				ib_datos_ok, ib_borrar, ib_ok, ib_traer, ib_deshace = True
Date					id_FechaAcceso
Time					it_HoraAcceso
end variables

forward prototypes
public function boolean wf_actualiza_db ()
end prototypes

event ue_antesguardar();Long	ll_fila 

For ll_Fila = 1 To dw_2.RowCount()
	If dw_2.IsSelected(ll_Fila) Then
		dw_2.Object.rfpe_porter[ll_Fila] = 1
	End If	
Next
end event

event ue_guardar();IF dw_2.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

If Not wf_actualiza_db() Then
	Message.DoubleParm = -1
	Return
End If
end event

event ue_nuevo();DateTime	ld_fechahora
Integer		li_filas

st_estado.Text	=	"Sin Conexión A La Base de Datos"
IF ib_connected THEN
	st_estado.Text	=	"Conectado Exitosamente A La Base de Datos"
	
	dw_1.Reset()
	dw_2.Reset()
	
	dw_1.SetTransObject(sqlca)
	dw_2.SetTransObject(sqlca)
	dw_2.ShareData(dw_1)
	
	li_filas	=	dw_2.Retrieve()
	
	DO
		IF li_filas = -1 THEN
			st_estado.Text	=	"Sin Conexión A La Base de Datos"
			Conexion()
	
			dw_1.SetTransObject(sqlca)
			dw_2.SetTransObject(sqlca)
			dw_2.ShareData(dw_1)
			
			li_Filas = dw_2.Retrieve()
			
		END IF
	LOOP WHILE li_filas = -1
	
	st_estado.Text	=	"Conectado Exitosamente A La Base de Datos"	
	li_filas	=	dw_2.Retrieve()
	
	dw_2.ScrollToRow(li_filas)
	dw_2.SetRow(li_filas)
	dw_2.SetFocus()
	dw_2.SetColumn(li_filas)
	dw_1.ScrollToRow(li_filas)
	dw_1.SetRow(li_filas)
END IF
end event

event ue_recuperadatos();long		ll_filas

SetPointer(HourGlass!)
PostEvent("ue_listo")

ll_Filas	=	dw_2.Retrieve()

If ll_Filas = -1 Then
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Movimiento Casino")
	dw_2.SetRedraw(True)
	Return
Else
	Timer(360)
End If

end event

public function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

ldt_FechaHora		=	F_FechaHora()

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

If dw_2.Update(True, False) = 1 Then 
	Commit;
	
	If sqlca.SQLCode <> 0 Then
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	Else
		lb_Retorno	=	True		
		dw_2.ResetUpdate()
	End If
Else
	RollBack;
	If sqlca.SQLCode <> 0 Then F_ErrorBaseDatos(sqlca, This.Title)
	lb_Retorno	=	False
End If

sqlca.AutoCommit	=	lb_AutoCommit

Return lb_Retorno
	

end function

on w_ingresocamiones_systray.create
int iCurrent
call super::create
this.dw_ticket=create dw_ticket
this.st_mensaje=create st_mensaje
this.st_estado=create st_estado
this.pb_lectura=create pb_lectura
this.pb_salir=create pb_salir
this.dw_2=create dw_2
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_ticket
this.Control[iCurrent+2]=this.st_mensaje
this.Control[iCurrent+3]=this.st_estado
this.Control[iCurrent+4]=this.pb_lectura
this.Control[iCurrent+5]=this.pb_salir
this.Control[iCurrent+6]=this.dw_2
this.Control[iCurrent+7]=this.dw_1
end on

on w_ingresocamiones_systray.destroy
call super::destroy
destroy(this.dw_ticket)
destroy(this.st_mensaje)
destroy(this.st_estado)
destroy(this.pb_lectura)
destroy(this.pb_salir)
destroy(this.dw_2)
destroy(this.dw_1)
end on

event open;call super::open;x					= 	0
y					= 	0
This.Width		= 	dw_1.width + 770
This.Height		= 	2750	

iuo_Camion			=	Create uo_TipoCamion	
iuo_Transportista	=	Create uo_Transportista

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_ticket.SetTransObject(sqlca)

dw_2.ShareData(dw_1)

This.PostEvent("ue_nuevo")

Timer(180)
end event

event resize;call super::resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0, &
			li_Ancho = 300, li_Alto = 245, li_Siguiente = 255


//This.WindowState	=	Maximized!

If dw_1.width > This.Width Then
	maximo		=	dw_1.width
Else
	dw_2.width	=	This.WorkSpaceWidth() - 100
	maximo		=	dw_2.width
End If

dw_1.x					=	37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.y					=	37

dw_2.x					=	37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					=	64 + dw_1.Height
dw_2.height				=	This.WorkSpaceHeight() - dw_2.y - 400

st_mensaje.x			=	dw_2.x
st_estado.x				=	dw_2.x
st_mensaje.y			=	64 + dw_2.y + dw_2.Height
st_estado.y				=	64 + st_mensaje.y + st_mensaje.Height
st_mensaje.width	 	=	dw_2.width	
st_estado.width	 	=	dw_2.width	


li_posic_x				=	This.WorkSpaceWidth() - 370
li_posic_y				=	170

IF pb_lectura.Visible THEN
	pb_lectura.x			=	li_posic_x
	pb_lectura.y			=	li_posic_y
	pb_lectura.width	=	li_Ancho
	pb_lectura.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_salir.Visible THEN
	pb_salir.x			=	li_posic_x
	pb_salir.y			=	li_posic_y
	pb_salir.width		=	li_Ancho
	pb_salir.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF
end event

event timer;call super::timer;DateTime	ld_fechahora
Integer		li_filas
DataStore	lds_pruebaconexion

Timer(0)
st_estado.Text	=	"Sin Conexión A La Base de Datos"

IF NOT ib_connected THEN
	Conexion()
END IF

IF ib_connected THEN
	st_estado.Text	=	"Conectado Exitosamente A La Base de Datos"
	
	dw_1.Reset()
	dw_2.Reset()
	
	dw_1.SetTransObject(sqlca)
	dw_2.SetTransObject(sqlca)
	dw_2.ShareData(dw_1)
	
	lds_pruebaconexion					=	Create DataStore
	lds_pruebaconexion.DataObject	=	dw_2.DataObject
	lds_pruebaconexion.SetTransObject(sqlca)
	
	li_filas	=	lds_pruebaconexion.Retrieve()
	
	DO
		IF li_filas = -1 THEN
			st_estado.Text	=	"Sin Conexión A La Base de Datos"
			Conexion()
	
			dw_1.SetTransObject(sqlca)
			dw_2.SetTransObject(sqlca)
			dw_2.ShareData(dw_1)
			lds_pruebaconexion.SetTransObject(sqlca)
			li_filas	=	lds_pruebaconexion.Retrieve()
			
		END IF
	LOOP WHILE li_filas = -1
	
	st_estado.Text	=	"Conectado Exitosamente A La Base de Datos"
	
	il_fila = dw_2.Retrieve()
	dw_2.ScrollToRow(il_fila)
	dw_2.SetRow(il_fila)
	dw_2.SetFocus()
	dw_2.SetColumn(il_fila)
	
	dw_1.ScrollToRow(il_fila)
	dw_1.SetRow(il_fila)

END IF

Timer(180)
end event

event ue_trayrightclicked;call super::ue_trayrightclicked;m_camiones_opcion lm_menu_opcion

This.SetFocus()
lm_menu_opcion = CREATE m_camiones_opcion
lm_menu_opcion.m_pop.PopMenu(PointerX(),PointerY())
DESTROY lm_menu_opcion
end event

type dw_ticket from datawindow within w_ingresocamiones_systray
boolean visible = false
integer x = 2834
integer y = 20
integer width = 123
integer height = 88
integer taborder = 30
string title = "none"
string dataobject = "dw_info_ticketingreso"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

type st_mensaje from statictext within w_ingresocamiones_systray
integer x = 41
integer y = 2212
integer width = 2862
integer height = 112
integer textsize = -18
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 255
long backcolor = 553648127
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_estado from statictext within w_ingresocamiones_systray
integer x = 41
integer y = 2364
integer width = 2862
integer height = 112
integer textsize = -18
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 255
long backcolor = 553648127
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type pb_lectura from picturebutton within w_ingresocamiones_systray
string tag = "Muestra Ultimo Ingreso"
integer x = 2743
integer y = 236
integer width = 302
integer height = 244
integer taborder = 10
integer textsize = -18
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean flatstyle = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
string powertiptext = "Muestra Ultimo Ingreso"
long backcolor = 553648127
end type

event clicked;Long			ll_Planta, ll_Numero
DateTime	ld_Fecha

Parent.PostEvent('ue_recuperadatos')

Parent.TriggerEvent('ue_guardar')

st_mensaje.Text = "Registro Actualizado."

If Message.DoubleParm = 0 Then
	If MessageBox("Atencion", "Desea Imprimir Comprobante.", Information!, YesNo!, 2) = 1 Then
		
		ll_PLanta 	=	dw_2.Object.plde_codigo[il_Fila]
		ll_Numero	=	dw_2.Object.rfpe_numero[il_Fila]		
		ld_Fecha	= Datetime(Today(),  Now())
		
		dw_Ticket.Retrieve(ll_Planta, ll_Numero, ld_Fecha)
	
		If dw_ticket.RowCount() > 0 Then 
			dw_ticket.Print(False, False)
			st_mensaje.Text = "Registro Impreso."
		Else
			st_mensaje.Text = "No se pudo emitir comprobante."
		End If
	End If
End If


end event

type pb_salir from picturebutton within w_ingresocamiones_systray
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2729
integer y = 516
integer width = 302
integer height = 244
integer taborder = 10
integer textsize = -18
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
boolean flatstyle = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Cancelar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Cancelar-bn.png"
long backcolor = 553648127
end type

event clicked;Parent.Hide()
end event

type dw_2 from datawindow within w_ingresocamiones_systray
integer x = 41
integer y = 956
integer width = 2656
integer height = 1204
integer taborder = 20
boolean titlebar = true
string title = "Detalle de Recepciones"
string dataobject = "dw_mues_ingresocamion"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;If Row > 0 Then
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
	
	dw_1.ScrollToRow(il_fila)
	dw_1.SetRow(il_fila)
End If

Return 0
end event

type dw_1 from datawindow within w_ingresocamiones_systray
integer x = 41
integer y = 28
integer width = 1934
integer height = 872
integer taborder = 20
string title = "none"
string dataobject = "dw_mant_ingresocamion"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_fila
string		ls_null, ls_columna

Timer(0)

SetNull(ls_null)

ls_columna	=	dwo.Name

Choose Case ls_columna
	Case "tica_codigo"
		If Not iuo_Camion.Existe(Long(Data), True, Sqlca) Then
			This.SetItem(Row, ls_columna, Integer(ls_null))
			Return 1			
		End If
		
	Case "tran_codigo"
		If Not iuo_Transportista.Existe(Long(Data), True, Sqlca) Then
			This.SetItem(Row, ls_columna, Integer(ls_null))
			Return 1			
		End If
		
End Choose 

Timer(180)	
end event

event itemerror;Return 1	
end event

