$PBExportHeader$w_cons_control_recepcion.srw
forward
global type w_cons_control_recepcion from window
end type
type st_5 from statictext within w_cons_control_recepcion
end type
type st_2 from statictext within w_cons_control_recepcion
end type
type dw_cliente from datawindow within w_cons_control_recepcion
end type
type dw_planta from datawindow within w_cons_control_recepcion
end type
type em_fecter from editmask within w_cons_control_recepcion
end type
type st_4 from statictext within w_cons_control_recepcion
end type
type pb_imprimir from picturebutton within w_cons_control_recepcion
end type
type pb_lectura from picturebutton within w_cons_control_recepcion
end type
type em_fecini from editmask within w_cons_control_recepcion
end type
type st_3 from statictext within w_cons_control_recepcion
end type
type dw_1 from uo_dw within w_cons_control_recepcion
end type
type pb_salir from picturebutton within w_cons_control_recepcion
end type
type pb_nuevo from picturebutton within w_cons_control_recepcion
end type
type gb_fecha from groupbox within w_cons_control_recepcion
end type
type st_encabe from statictext within w_cons_control_recepcion
end type
end forward

global type w_cons_control_recepcion from window
integer x = 5
integer y = 16
integer width = 4393
integer height = 2076
boolean titlebar = true
string title = "Consulta Control de Recepción"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 16777215
event ue_asignacion ( )
st_5 st_5
st_2 st_2
dw_cliente dw_cliente
dw_planta dw_planta
em_fecter em_fecter
st_4 st_4
pb_imprimir pb_imprimir
pb_lectura pb_lectura
em_fecini em_fecini
st_3 st_3
dw_1 dw_1
pb_salir pb_salir
pb_nuevo pb_nuevo
gb_fecha gb_fecha
st_encabe st_encabe
end type
global w_cons_control_recepcion w_cons_control_recepcion

type variables
String   ls_año_periodo
Long 		il_fila
Date		id_FechaAcceso
Time		it_HoraAcceso
Integer	ii_NroMes

DataWindowChild	idwc_cliente, idwc_planta

uo_clientesprod	iuo_clienteprod
uo_plantadesp	iuo_plantadesp

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

dw_1.AcceptText()
lstr_busq.argum[1]	=	String(dw_1.Object.plde_codigo[il_fila])
lstr_busq.argum[2]	=	String(dw_1.Object.rfpe_numero[il_fila])
lstr_busq.argum[3]	=	String(dw_1.Object.clie_codigo[il_fila])

IF dw_1.Object.spcr_estado[il_fila] = 1 THEN
	OpenSheetWithParm(w_cons_recfruprocee_particular, lstr_busq, w_main, columna, Original!)
ELSE
	OpenSheetWithParm(w_cons_recfruprocee_particular_trans, lstr_busq, w_main, columna, Original!)
END IF


end event

on w_cons_control_recepcion.create
this.st_5=create st_5
this.st_2=create st_2
this.dw_cliente=create dw_cliente
this.dw_planta=create dw_planta
this.em_fecter=create em_fecter
this.st_4=create st_4
this.pb_imprimir=create pb_imprimir
this.pb_lectura=create pb_lectura
this.em_fecini=create em_fecini
this.st_3=create st_3
this.dw_1=create dw_1
this.pb_salir=create pb_salir
this.pb_nuevo=create pb_nuevo
this.gb_fecha=create gb_fecha
this.st_encabe=create st_encabe
this.Control[]={this.st_5,&
this.st_2,&
this.dw_cliente,&
this.dw_planta,&
this.em_fecter,&
this.st_4,&
this.pb_imprimir,&
this.pb_lectura,&
this.em_fecini,&
this.st_3,&
this.dw_1,&
this.pb_salir,&
this.pb_nuevo,&
this.gb_fecha,&
this.st_encabe}
end on

on w_cons_control_recepcion.destroy
destroy(this.st_5)
destroy(this.st_2)
destroy(this.dw_cliente)
destroy(this.dw_planta)
destroy(this.em_fecter)
destroy(this.st_4)
destroy(this.pb_imprimir)
destroy(this.pb_lectura)
destroy(this.em_fecini)
destroy(this.st_3)
destroy(this.dw_1)
destroy(this.pb_salir)
destroy(this.pb_nuevo)
destroy(this.gb_fecha)
destroy(this.st_encabe)
end on

event open;im_menu		= m_principal

This.Icon										=	Gstr_apl.Icono
This.Width									= dw_1.width + 540
This.Height									= 2500
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve(gi_CodExport)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)

iuo_clienteprod	 =	Create uo_clientesprod
iuo_plantadesp =	Create uo_plantadesp

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

em_fecini.SetFocus()
em_fecini.text					=	String(RelativeDate(Today() , -365))
em_fecter.text					=	String(Today())	

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

type st_5 from statictext within w_cons_control_recepcion
integer x = 251
integer y = 240
integer width = 215
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_cons_control_recepcion
integer x = 251
integer y = 132
integer width = 229
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
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_cons_control_recepcion
integer x = 544
integer y = 112
integer width = 1157
integer height = 96
integer taborder = 10
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemerror;RETURN 1
end event

event itemchanged;String  ls_null

SetNull(ls_null)

IF NOT iuo_clienteprod.existe(Integer(data),True,sqlca) THEN
		dw_cliente.Object.clie_codigo[1] =  Integer(ls_null)
		RETURN 1	
END IF
end event

type dw_planta from datawindow within w_cons_control_recepcion
integer x = 544
integer y = 224
integer width = 978
integer height = 88
integer taborder = 20
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemerror;RETURN 1
end event

event itemchanged;String  ls_null

SetNull(ls_null)

IF NOT iuo_plantadesp.ExisteFrigo(Integer(data),True,sqlca) THEN
		dw_planta.Object.plde_codigo[1] =  Integer(ls_null)
		RETURN 1	
END IF
end event

type em_fecter from editmask within w_cons_control_recepcion
integer x = 3099
integer y = 176
integer width = 425
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

type st_4 from statictext within w_cons_control_recepcion
integer x = 2853
integer y = 188
integer width = 256
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
string text = "Termino"
boolean focusrectangle = false
end type

type pb_imprimir from picturebutton within w_cons_control_recepcion
string tag = "Selección de Parámetros"
integer x = 3913
integer y = 788
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

event clicked;Long		fila
String	ls_Mascara
Date		ld_FechaCons

SetPointer(HourGlass!)

istr_info.titulo	=	"CONTROL RECEPCION"
istr_info.copias	=	1

OpenWithParm(vinf, istr_info)
	
vinf.dw_1.DataObject = "dw_info_controlrecepcion"

vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(dw_cliente.Object.clie_codigo[1],dw_planta.Object.plde_codigo[1],Date(em_fecini.text),Date(em_fecter.text))

IF fila = -1 OR fila = 0 THEN
	IF fila = -1 THEN	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	IF fila = 0 THEN MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_lectura from picturebutton within w_cons_control_recepcion
string tag = "Selección de Parámetros"
integer x = 3913
integer y = 540
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
IF IsNull(dw_cliente.Object.clie_codigo[1]) THEN 
	MessageBox('Atención','Debe Ingresar un Cliente Previamente')
	RETURN
END IF

IF IsNull(dw_planta.Object.plde_codigo[1]) THEN
	MessageBox('Atención','Debe Ingresar un Planta Previamente')
	RETURN
END IF

IF IsNull(Date(em_fecini.text)) OR Date(em_fecini.text) =Date( '01/01/1900') THEN
	MessageBox('Atención','Debe Ingresar un Fecha Inicio')
	RETURN
END IF

IF IsNull(Date(em_fecter.text)) OR Date(em_fecter.text) = Date( '01/01/1900') THEN
	MessageBox('Atención','Debe Ingresar Fecha Termino')
	RETURN
END IF

dw_1.Retrieve(dw_cliente.Object.clie_codigo[1],dw_planta.Object.plde_codigo[1],Date(em_fecini.text),Date(em_fecter.text))
	
dw_1.SetRowFocusIndicator(Hand!)
dw_1.SetFocus()
		
IF dw_1.RowCount() = 0 THEN
	MessageBox('ADVERTENCIA','No Existe información para este período')
	RETURN
ELSE
	pb_imprimir.Enabled	=	True
END IF
end event

type em_fecini from editmask within w_cons_control_recepcion
integer x = 2382
integer y = 176
integer width = 425
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

type st_3 from statictext within w_cons_control_recepcion
integer x = 2194
integer y = 188
integer width = 183
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
string text = "Inicio"
boolean focusrectangle = false
end type

type dw_1 from uo_dw within w_cons_control_recepcion
integer x = 37
integer y = 388
integer width = 3790
integer height = 1400
integer taborder = 100
boolean titlebar = true
string title = " Detalle de Recepciones"
string dataobject = "dw_mues_controlrecepcion"
boolean hscrollbar = true
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
	
	m_consulta.m_m_edicion.consultarecepcion.Visible									=	True
//	
//	IF This.Object.rfpe_numerot[il_fila] > 0 THEN
//		m_consulta.m_m_edicion.m_ctacte.Visible									=	True
//		m_consulta.m_m_edicion.resumencuentacorrientecuenta.Visible			=	True
//	END IF
	
//	m_consulta.m_m_edicion.consultacomprobante.Enabled = 	False
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

type pb_salir from picturebutton within w_cons_control_recepcion
integer x = 3913
integer y = 1536
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
alignment htextalign = left!
end type

event pb_salir::clicked;call super::clicked;Close(parent)
end event

type pb_nuevo from picturebutton within w_cons_control_recepcion
string tag = "Selección de Parámetros"
integer x = 3918
integer y = 1264
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
pb_imprimir.Enabled	=	False
em_fecini.SetFocus()
end event

type gb_fecha from groupbox within w_cons_control_recepcion
integer x = 2153
integer y = 108
integer width = 1403
integer height = 196
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Recepción"
end type

type st_encabe from statictext within w_cons_control_recepcion
integer x = 37
integer y = 48
integer width = 3790
integer height = 320
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

