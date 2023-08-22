$PBExportHeader$w_cons_cierre_despachos.srw
forward
global type w_cons_cierre_despachos from window
end type
type st_5 from statictext within w_cons_cierre_despachos
end type
type em_fechater from editmask within w_cons_cierre_despachos
end type
type st_3 from statictext within w_cons_cierre_despachos
end type
type dw_planta from datawindow within w_cons_cierre_despachos
end type
type st_2 from statictext within w_cons_cierre_despachos
end type
type dw_cliente from datawindow within w_cons_cierre_despachos
end type
type pb_imprimir from picturebutton within w_cons_cierre_despachos
end type
type pb_lectura from picturebutton within w_cons_cierre_despachos
end type
type em_fechaini from editmask within w_cons_cierre_despachos
end type
type st_4 from statictext within w_cons_cierre_despachos
end type
type dw_1 from uo_dw within w_cons_cierre_despachos
end type
type pb_salir from picturebutton within w_cons_cierre_despachos
end type
type pb_nuevo from picturebutton within w_cons_cierre_despachos
end type
type st_encabe from statictext within w_cons_cierre_despachos
end type
end forward

global type w_cons_cierre_despachos from window
integer x = 5
integer y = 16
integer width = 5111
integer height = 2184
boolean titlebar = true
string title = "Consulta Validación Cierre Despachos"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 16777215
event ue_asignacion ( )
st_5 st_5
em_fechater em_fechater
st_3 st_3
dw_planta dw_planta
st_2 st_2
dw_cliente dw_cliente
pb_imprimir pb_imprimir
pb_lectura pb_lectura
em_fechaini em_fechaini
st_4 st_4
dw_1 dw_1
pb_salir pb_salir
pb_nuevo pb_nuevo
st_encabe st_encabe
end type
global w_cons_cierre_despachos w_cons_cierre_despachos

type variables
String   ls_año_periodo
Long 		il_fila
Date		id_FechaAcceso
Time		it_HoraAcceso
Integer	ii_NroMes

DataWindowChild	idwc_cliente, idwc_planta
Menu		im_menu

Str_parms		istr_parms
Str_mant			istr_mant
Str_busqueda	istr_busq
Str_info			istr_info
end variables

event ue_asignacion();String				ls_mes_periodo
Integer				columna, li_fila
Str_busqueda		lstr_busq

il_fila	=	dw_1.GetRow()

CHOOSE CASE gs_windows
	CASE '1'
			dw_1.AcceptText()
			lstr_busq.argum[1]	=	String(dw_1.Object.plde_codigo[il_Fila])
			lstr_busq.argum[2]	=	String(dw_1.Object.defe_numero[il_fila])
			lstr_busq.argum[3]	=	String(dw_1.Object.clie_codigo[il_Fila])
			OpenSheetWithParm(w_cons_maed_despafrigoen, lstr_busq, w_main, columna, Original!)
	
//	CASE '2'
//			lstr_busq.argum[1]	=	st_periodo.Text
//			lstr_busq.argum[3]	=	dw_1.Object.pcta_cuenta[il_fila]
//			lstr_busq.argum[4]	=	dw_1.Object.pcta_descri[il_fila]
//			lstr_busq.argum[2]	=	String(ii_NroMes)
//			OpenSheetWithParm(w_cons_mayor_por_cuenta, lstr_busq, w_main, columna, Original!)
//			
//	CASE '5'
//			lstr_busq.argum[1]	=	st_periodo.Text
//			lstr_busq.argum[2]	=	dw_1.Object.pcta_cuenta[il_fila]
//			lstr_busq.argum[5]	=	dw_1.Object.pcta_descri[il_fila]
//			lstr_busq.argum[6]	=	''
//			lstr_busq.argum[7]	=	''
//			OpenSheetWithParm(w_cons_contctactehist_resumencuenta, lstr_busq, w_main, columna, Original!)

END CHOOSE

	
end event

on w_cons_cierre_despachos.create
this.st_5=create st_5
this.em_fechater=create em_fechater
this.st_3=create st_3
this.dw_planta=create dw_planta
this.st_2=create st_2
this.dw_cliente=create dw_cliente
this.pb_imprimir=create pb_imprimir
this.pb_lectura=create pb_lectura
this.em_fechaini=create em_fechaini
this.st_4=create st_4
this.dw_1=create dw_1
this.pb_salir=create pb_salir
this.pb_nuevo=create pb_nuevo
this.st_encabe=create st_encabe
this.Control[]={this.st_5,&
this.em_fechater,&
this.st_3,&
this.dw_planta,&
this.st_2,&
this.dw_cliente,&
this.pb_imprimir,&
this.pb_lectura,&
this.em_fechaini,&
this.st_4,&
this.dw_1,&
this.pb_salir,&
this.pb_nuevo,&
this.st_encabe}
end on

on w_cons_cierre_despachos.destroy
destroy(this.st_5)
destroy(this.em_fechater)
destroy(this.st_3)
destroy(this.dw_planta)
destroy(this.st_2)
destroy(this.dw_cliente)
destroy(this.pb_imprimir)
destroy(this.pb_lectura)
destroy(this.em_fechaini)
destroy(this.st_4)
destroy(this.dw_1)
destroy(this.pb_salir)
destroy(this.pb_nuevo)
destroy(this.st_encabe)
end on

event open;x	=	0
y	=	0

im_menu		= m_principal

This.Icon										=	Gstr_apl.Icono
This.Width									= dw_1.width + 540
This.Height									= 2500
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True


dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1,"clie_codigo",gi_codexport)

dw_Planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_Planta.InsertRow(0)
dw_Planta.SetItem(1,"plde_codigo",gi_codplanta)

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

em_fechaini.Text			=	String(RelativeDate(Today(),-365))
em_fechaTer.Text			=	String(Today())

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
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

pb_salir.x				=	li_posic_x
pb_salir.y				=	dw_1.y + dw_1.Height - li_Siguiente
pb_salir.width			=	li_Ancho
pb_salir.height			=	li_Alto
end event

type st_5 from statictext within w_cons_cierre_despachos
integer x = 1198
integer y = 244
integer width = 453
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Fecha Término"
boolean focusrectangle = false
end type

type em_fechater from editmask within w_cons_cierre_despachos
integer x = 1641
integer y = 236
integer width = 407
integer height = 92
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_3 from statictext within w_cons_cierre_despachos
integer x = 2464
integer y = 116
integer width = 402
integer height = 64
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

type dw_planta from datawindow within w_cons_cierre_despachos
integer x = 2871
integer y = 100
integer width = 1083
integer height = 100
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[2]	=	data
end event

type st_2 from statictext within w_cons_cierre_despachos
integer x = 334
integer y = 116
integer width = 402
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

type dw_cliente from datawindow within w_cons_cierre_despachos
integer x = 722
integer y = 100
integer width = 1230
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;idwc_planta.Retrieve(1)

end event

type pb_imprimir from picturebutton within w_cons_cierre_despachos
string tag = "Selección de Parámetros"
boolean visible = false
integer x = 4690
integer y = 860
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

event clicked;//Long		fila
//String	ls_Mascara
//Date		ld_FechaCons
//
//SetPointer(HourGlass!)
//
//istr_info.titulo	=	"SALDOS POR CUENTA"
//istr_info.copias	=	1
//ld_FechaCons		=	Date('01/' + em_mes.Text)
////ls_Mascara			=	F_Global_Replace(gstr_param.mas_cuenta, "X", "@")
//
//OpenWithParm(vinf, istr_info)
//
//vinf.dw_1.DataObject = "dw_info_cons_saldos_por_cuenta"
//
//vinf.dw_1.SetTransObject(sqlca)
//
//fila	=	vinf.dw_1.Retrieve(ld_FechaCons, ls_Mascara)
//
//IF fila = -1 OR fila = 0 THEN
//	IF fila = -1 THEN	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
//					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
//	IF fila = 0 THEN MessageBox( "No Existe información", "No existe información para este informe.", &
//					StopSign!, Ok!)
//ELSE
//	F_Membrete(vinf.dw_1)
//	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
//	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
//
//	vinf.Visible	= True
//	vinf.Enabled	= True
//END IF
//
//SetPointer(Arrow!)
end event

type pb_lectura from picturebutton within w_cons_cierre_despachos
string tag = "Selección de Parámetros"
integer x = 4667
integer y = 552
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

event clicked;
il_fila 	=	dw_1.Retrieve(dw_cliente.Object.clie_codigo[1],dw_Planta.Object.plde_codigo[1],Date(em_FechaIni.Text),Date(em_FechaTer.Text))
	
dw_1.SetRowFocusIndicator(Hand!)
dw_1.SetFocus()
		
IF il_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF il_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para esta Consulta." , Exclamation!, Ok!)
	RETURN
ELSE
	pb_imprimir.Enabled	=	True
END IF
end event

type em_fechaini from editmask within w_cons_cierre_despachos
integer x = 727
integer y = 236
integer width = 407
integer height = 92
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_4 from statictext within w_cons_cierre_despachos
integer x = 334
integer y = 244
integer width = 370
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Fecha Inicio"
boolean focusrectangle = false
end type

type dw_1 from uo_dw within w_cons_cierre_despachos
integer x = 37
integer y = 384
integer width = 4562
integer height = 1652
integer taborder = 90
boolean titlebar = true
string title = "Validación de Despachos"
string dataobject = "dw_cons_cierre_despachos"
boolean hscrollbar = true
boolean livescroll = true
end type

event constructor;SetTransObject(sqlca)


end event

event rbuttondown;m_consulta	l_Menu

IF RowCount() =	0	THEN
	Return
ELSE
	gstr_us.OpcionActiva	=	Parent.ClassName()
	il_fila 					=	Row
	This.SetRow(il_fila)
	
	l_Menu = CREATE m_consulta
	
	m_consulta.m_m_edicion.DetaDespacho.Visible									=	True
	l_Menu.m_m_edicion.PopMenu(This.PointerX(),This.PointerY()+750)
	
END IF

end event

event doubleclicked;//IF Row <> 0 THEN
//	gs_windows		=	'1'
//	gs_opcion		=  '2'
//	Parent.TriggerEvent('ue_asignacion')
//	This.SetFocus()
//END IF
end event

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

type pb_salir from picturebutton within w_cons_cierre_despachos
integer x = 4686
integer y = 1704
integer width = 302
integer height = 244
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\SalirDisab.png"
alignment htextalign = left!
end type

event pb_salir::clicked;call super::clicked;Close(parent)
end event

type pb_nuevo from picturebutton within w_cons_cierre_despachos
string tag = "Selección de Parámetros"
integer x = 4681
integer y = 1380
integer width = 302
integer height = 244
integer taborder = 70
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
em_FechaIni.Text				=	''
em_FechaTer.Text			=	''
pb_imprimir.Enabled	=	False
em_FechaIni.SetFocus()
end event

type st_encabe from statictext within w_cons_cierre_despachos
integer x = 37
integer y = 60
integer width = 4562
integer height = 300
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 134217751
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

