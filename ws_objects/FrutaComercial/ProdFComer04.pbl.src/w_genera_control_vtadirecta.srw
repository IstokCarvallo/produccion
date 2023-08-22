$PBExportHeader$w_genera_control_vtadirecta.srw
forward
global type w_genera_control_vtadirecta from w_mant_tabla
end type
type st_4 from statictext within w_genera_control_vtadirecta
end type
type em_fecha from editmask within w_genera_control_vtadirecta
end type
type st_1 from statictext within w_genera_control_vtadirecta
end type
type em_porcentaje from editmask within w_genera_control_vtadirecta
end type
type cbx_informe from checkbox within w_genera_control_vtadirecta
end type
type st_3 from statictext within w_genera_control_vtadirecta
end type
type st_5 from statictext within w_genera_control_vtadirecta
end type
type st_6 from statictext within w_genera_control_vtadirecta
end type
type st_2 from statictext within w_genera_control_vtadirecta
end type
type st_7 from statictext within w_genera_control_vtadirecta
end type
type uo_selproductor from uo_seleccion_productor within w_genera_control_vtadirecta
end type
type uo_selcliente from uo_seleccion_clientesprod within w_genera_control_vtadirecta
end type
end forward

global type w_genera_control_vtadirecta from w_mant_tabla
integer width = 2597
integer height = 792
string title = "Genera Gestión Venta"
boolean resizable = false
boolean clientedge = true
event ue_cargadatos ( )
st_4 st_4
em_fecha em_fecha
st_1 st_1
em_porcentaje em_porcentaje
cbx_informe cbx_informe
st_3 st_3
st_5 st_5
st_6 st_6
st_2 st_2
st_7 st_7
uo_selproductor uo_selproductor
uo_selcliente uo_selcliente
end type
global w_genera_control_vtadirecta w_genera_control_vtadirecta

type variables

end variables

forward prototypes
protected function integer wf_modifica ()
protected function boolean wf_actualiza_db ()
end prototypes

event ue_cargadatos();//Long 		ll_Fila, ll_nuevo
//String 	ls_cliente, ls_planta, ls_especie, ls_variedad, ls_productor
//String		ls_mespro, ls_semana, ls_categoria, ls_busqueda
//
//For ll_Fila = 1 To dw_2.RowCount()
//	ls_cliente   		= String(dw_2.Object.clie_codigo[ll_Fila])
//	ls_planta   		= String(dw_2.Object.lofc_pltcod[ll_Fila])
//	ls_especie 		= String(dw_2.Object.lofc_espcod[ll_Fila])
//	ls_variedad		= String(dw_2.Object.vari_codigo[ll_Fila])
//	ls_productor		= String(dw_2.Object.prod_codigo[ll_Fila])
//	ls_mespro		= '01/' + em_fecha.Text
//	ls_categoria		= String(dw_2.Object.cate_codigo[ll_Fila])
//	ls_semana		= String(dw_2.Object.semana[ll_Fila])
//		
//	ls_busqueda		=	"clie_codigo = " + ls_cliente + 	" AND " + &
//							"plde_codigo = " + ls_planta + 	" AND " + &
//							"espe_codigo = " + ls_especie 	+ 	" AND " + &
//							"vari_codigo = " + ls_variedad 	+ 	" AND " + & 
//							"prod_codigo = " + ls_productor + 	" AND " + &
//							"fpfc_semana = " + ls_semana + 	" AND " + &
//							"cate_codigo = " + ls_categoria  
//	ll_nuevo = dw_1.Find(ls_busqueda, 1, dw_1.RowCount())
//								
//	If ll_nuevo = 0 Then
//		ll_Nuevo = dw_1.InsertRow(0)
//		dw_1.Object.clie_codigo[ll_Nuevo]	= dw_2.Object.clie_codigo[ll_Fila]
//		dw_1.Object.plde_codigo[ll_Nuevo]	= dw_2.Object.lofc_pltcod[ll_Fila]
//		dw_1.Object.espe_codigo[ll_Nuevo]	= dw_2.Object.lofc_espcod[ll_Fila]
//		dw_1.Object.vari_codigo[ll_Nuevo]	= dw_2.Object.vari_codigo[ll_Fila]
//		dw_1.Object.prod_codigo[ll_Nuevo]	= dw_2.Object.prod_codigo[ll_Fila]
//		dw_1.Object.fpfc_mespro[ll_Nuevo]	= Date(ls_mespro)
//		dw_1.Object.cate_codigo[ll_Nuevo]	= dw_2.Object.cate_codigo[ll_Fila]
//		dw_1.Object.fpfc_semana[ll_Nuevo]	= dw_2.Object.semana[ll_Fila]
//	End If
//	
//	dw_1.Object.fpfc_kilven[ll_Nuevo]		= dw_2.Object.Kilos[ll_Fila]
//	dw_1.Object.fpfc_prepro[ll_Nuevo]	= dw_2.Object.promedio[ll_Fila]
//	dw_1.Object.fpfc_porcen[ll_Nuevo]	= dw_2.Object.porcentaje[ll_Fila]
//	dw_1.Object.fpfc_preppt[ll_Nuevo]	= dw_2.Object.precio_compra_prod[ll_Fila]
//	dw_1.Object.fpfc_valpor[ll_Nuevo]		= dw_2.Object.gastos_adm[ll_Fila]
//	
////	dw_1.SetSort("clie_codigo, plde_codigo, espe_codigo, vari_codigo, prod_codigo, fpfc_semana, cate_codigo")
////	dw_1.Sort()
//Next
//	
//This.TriggerEvent("ue_guardar")
end event

protected function integer wf_modifica ();if dw_1.accepttext() = -1 then return -1
if (dw_1.modifiedcount() + dw_1.deletedcount()) > 0 then return 0
return 1
end function

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_1.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_1.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
			
		dw_1.ResetUpdate()
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
Date		ld_Hasta
str_info	lstr_info

lstr_info.titulo	= "MAESTRO VENTA DIRECTA FRUTA COMERCIAL"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_promedio_precio_venta"
vinf.dw_1.SetTransObject(sqlca)
ld_Hasta = RelativeDate(date('01/' + em_fecha.Text), f_diasdelmes(date('01/' + em_fecha.Text)) - 1)

fila = vinf.dw_1.Retrieve(date('01/' + em_fecha.Text), ld_hasta, -1, uo_SelCliente.Codigo, uo_SelProductor.Codigo, Long(em_porcentaje.Text))

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Object.DataWindow.Zoom = 95
	vinf.dw_1.ModIfy('DataWindow.Print.Preview = Yes')
	vinf.dw_1.ModIfy('DataWindow.Print.Preview.Zoom = 75')
	vinf.Visible	= True
	vinf.Enabled	= True
End If

SetPointer(Arrow!)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta, ll_filas, ll_Flag = 0
Date	ld_Fecha
Dec{2} ld_Porcentaje

Do
	ld_Fecha	=	date('01/' + em_fecha.Text)
	ld_Porcentaje = Dec(em_porcentaje.Text)
	
	Declare GeneraFactura Procedure For dbo.FComer_Genera_FacturaComercial
			@Fecha			=	:ld_Fecha, 
			@cliente			=	:uo_SelCliente.Codigo, 
			@Productor		=	:uo_SelProductor.Codigo,
			@Porcentaje	=	:ld_Porcentaje			
	Using	Sqlca;
					
	Execute GeneraFactura ;
					
	If sqlca.SQLCode = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
		pb_grabar.Enabled = True
	Else
		If cbx_informe.Checked Then This.TriggerEvent("ue_imprimir")
	End If
	
	Close GeneraFactura ;
	Commit;
Loop While respuesta = 1

SetPointer(Arrow!)

If respuesta = 2 Then
	Close(This)
End If
end event

on w_genera_control_vtadirecta.create
int iCurrent
call super::create
this.st_4=create st_4
this.em_fecha=create em_fecha
this.st_1=create st_1
this.em_porcentaje=create em_porcentaje
this.cbx_informe=create cbx_informe
this.st_3=create st_3
this.st_5=create st_5
this.st_6=create st_6
this.st_2=create st_2
this.st_7=create st_7
this.uo_selproductor=create uo_selproductor
this.uo_selcliente=create uo_selcliente
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.em_fecha
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.em_porcentaje
this.Control[iCurrent+5]=this.cbx_informe
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.st_5
this.Control[iCurrent+8]=this.st_6
this.Control[iCurrent+9]=this.st_2
this.Control[iCurrent+10]=this.st_7
this.Control[iCurrent+11]=this.uo_selproductor
this.Control[iCurrent+12]=this.uo_selcliente
end on

on w_genera_control_vtadirecta.destroy
call super::destroy
destroy(this.st_4)
destroy(this.em_fecha)
destroy(this.st_1)
destroy(this.em_porcentaje)
destroy(this.cbx_informe)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.st_2)
destroy(this.st_7)
destroy(this.uo_selproductor)
destroy(this.uo_selcliente)
end on

event open;x				= 0
y				= 0

im_menu	= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_1.ModIfy("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.ModIfy("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)

Boolean	lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_SelProductor.Codigo) Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelCliente.Inicia(Gi_CodExport)
	
	uo_SelProductor.Seleccion(True, False)
	uo_SelProductor.Filtra(-1)
	pb_nuevo.PostEvent(Clicked!)
END If
end event

event ue_listo();//
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	MessageBox("Atención","Información se Generó satisfactoriamente")
	IF cbx_Informe.Checked THEN This.TriggerEvent("ue_imprimir")
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF


end event

event resize;//
end event

type dw_1 from w_mant_tabla`dw_1 within w_genera_control_vtadirecta
boolean visible = false
integer x = 73
integer y = 700
integer width = 1979
integer height = 672
integer taborder = 0
boolean titlebar = true
string dataobject = "dw_mues_spro_facturprocomercial"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean hscrollbar = true
end type

event dw_1::clicked;//Integer 	li_aviso, ll_fila=0
//
//IF Row > 0 THEN
//	
//	il_fila = Row
//	This.SelectRow(0,False)
//	This.SetRow(il_fila)
//	This.SelectRow(il_fila,True)
//
//	IF dw_2.RowCount() > 0 THEN
//		IF dw_2.ModifiedCount() > 0 THEN
//			IF MessageBox("Datos Pendientes de Grabación","Desea grabar los avisos",Question!,YesNo!,1) = 1 THEN
//				Parent.TriggerEvent("ue_guardar")
//				IF Message.DoubleParm = -1 THEN RETURN
//			END IF
//		END IF
//	END IF
//	
//	li_aviso = NroAviso(dw_1.Object.orpr_numero[Row])
//	dw_2.Reset()	
//	
//	IF li_Aviso > 1 THEN
//		dw_2.Retrieve(Integer(Istr_Mant.Argumento[1]),Integer(Istr_Mant.Argumento[2]), &
//						  dw_1.Object.orpr_numero[Row])
//	END IF								
//	
//	IF li_Aviso > 0 THEN
//		pb_grabar.Enabled	=	TRUE
//		il_fila_e = dw_2.InsertRow(0)
//		dw_2.ScrollToRow(il_fila_e)
//		dw_2.SetRow(il_fila_e)
//		dw_2.SetItem(il_fila_e,"plde_codigo",Integer(istr_Mant.Argumento[1]))
//		dw_2.SetItem(il_fila_e,"orpr_tipord",Integer(istr_Mant.Argumento[2]))
//		dw_2.SetItem(il_fila_e,"orpr_numero",dw_1.Object.orpr_numero[Row])
//		dw_2.SetItem(il_fila_e,"orpa_nroavi",li_Aviso)		
//      
//		dw_2.SetFocus()
//	END IF 
//	
//END IF
//
//RETURN 0
end event

event dw_1::doubleclicked;//
end event

event dw_1::sqlpreview;//
end event

type st_encabe from w_mant_tabla`st_encabe within w_genera_control_vtadirecta
integer x = 55
integer y = 44
integer width = 2080
integer height = 408
boolean enabled = true
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_genera_control_vtadirecta
boolean visible = false
integer x = 2203
integer y = 1352
integer taborder = 0
boolean enabled = false
end type

event pb_lectura::clicked;//
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_genera_control_vtadirecta
boolean visible = false
integer x = 2203
integer y = 1208
integer taborder = 0
boolean enabled = false
end type

type pb_insertar from w_mant_tabla`pb_insertar within w_genera_control_vtadirecta
boolean visible = false
integer x = 2203
integer y = 1496
integer taborder = 0
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_genera_control_vtadirecta
boolean visible = false
integer x = 2318
integer y = 1592
integer taborder = 0
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_genera_control_vtadirecta
integer x = 2194
integer y = 44
integer weight = 400
fontcharset fontcharset = ansi!
string pointer = "AppStarting!"
boolean enabled = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
long backcolor = 553648127
end type

event pb_grabar::clicked;Parent.TriggerEvent("ue_recuperadatos")
end event

type pb_imprimir from w_mant_tabla`pb_imprimir within w_genera_control_vtadirecta
boolean visible = false
integer x = 2203
integer y = 932
integer taborder = 0
end type

type pb_salir from w_mant_tabla`pb_salir within w_genera_control_vtadirecta
integer x = 2199
integer y = 408
integer taborder = 70
string pointer = "AppStarting!"
long backcolor = 553648127
end type

type st_4 from statictext within w_genera_control_vtadirecta
integer x = 1330
integer y = 120
integer width = 366
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Mes Proceso"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_genera_control_vtadirecta
string tag = "Fecha Inicio"
integer x = 1705
integer y = 116
integer width = 398
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "mm/yyyy"
boolean dropdowncalendar = true
end type

type st_1 from statictext within w_genera_control_vtadirecta
integer x = 151
integer y = 552
integer width = 361
integer height = 84
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
string text = "Comisión"
boolean focusrectangle = false
end type

type em_porcentaje from editmask within w_genera_control_vtadirecta
integer x = 535
integer y = 504
integer width = 288
integer height = 92
integer taborder = 40
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
maskdatatype maskdatatype = decimalmask!
string mask = "00.00"
end type

type cbx_informe from checkbox within w_genera_control_vtadirecta
integer x = 1280
integer y = 508
integer width = 603
integer height = 80
integer taborder = 50
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Genera Reporte"
boolean lefttext = true
boolean righttoleft = true
end type

type st_3 from statictext within w_genera_control_vtadirecta
integer x = 1134
integer y = 452
integer width = 1001
integer height = 200
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_5 from statictext within w_genera_control_vtadirecta
integer x = 151
integer y = 468
integer width = 361
integer height = 84
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
string text = "Porcentaje"
boolean focusrectangle = false
end type

type st_6 from statictext within w_genera_control_vtadirecta
integer x = 55
integer y = 452
integer width = 1074
integer height = 200
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_genera_control_vtadirecta
integer x = 105
integer y = 128
integer width = 224
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

type st_7 from statictext within w_genera_control_vtadirecta
integer x = 105
integer y = 320
integer width = 297
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

type uo_selproductor from uo_seleccion_productor within w_genera_control_vtadirecta
event destroy ( )
integer x = 416
integer y = 228
integer taborder = 30
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_genera_control_vtadirecta
event destroy ( )
integer x = 416
integer y = 112
integer height = 96
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

