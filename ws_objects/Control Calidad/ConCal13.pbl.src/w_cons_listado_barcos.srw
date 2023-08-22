$PBExportHeader$w_cons_listado_barcos.srw
 forward
global type w_cons_listado_barcos from window
end type
type uo_muestradest from uo_seleccion_destinos_mod within w_cons_listado_barcos
end type
type em_fecha from editmask within w_cons_listado_barcos
end type
type st_3 from statictext within w_cons_listado_barcos
end type
type uo_muestratipo from uo_seleccion_tipotransporte_mod within w_cons_listado_barcos
end type
type st_5 from statictext within w_cons_listado_barcos
end type
type uo_muestranave from uo_seleccion_naves_mod within w_cons_listado_barcos
end type
type st_6 from statictext within w_cons_listado_barcos
end type
type pb_imprimir from picturebutton within w_cons_listado_barcos
end type
type pb_2 from picturebutton within w_cons_listado_barcos
end type
type st_2 from statictext within w_cons_listado_barcos
end type
type dw_1 from uo_dw within w_cons_listado_barcos
end type
type pb_salir from picturebutton within w_cons_listado_barcos
end type
type pb_1 from picturebutton within w_cons_listado_barcos
end type
type gb_4 from groupbox within w_cons_listado_barcos
end type
type gb_5 from groupbox within w_cons_listado_barcos
end type
type st_1 from statictext within w_cons_listado_barcos
end type
end forward

global type w_cons_listado_barcos from window
integer x = 5
integer y = 16
integer width = 3598
integer height = 2020
boolean titlebar = true
string title = "Consulta Detalle Nave"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 12632256
event ue_asignacion ( )
uo_muestradest uo_muestradest
em_fecha em_fecha
st_3 st_3
uo_muestratipo uo_muestratipo
st_5 st_5
uo_muestranave uo_muestranave
st_6 st_6
pb_imprimir pb_imprimir
pb_2 pb_2
st_2 st_2
dw_1 dw_1
pb_salir pb_salir
pb_1 pb_1
gb_4 gb_4
gb_5 gb_5
st_1 st_1
end type
global w_cons_listado_barcos w_cons_listado_barcos

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

event ue_asignacion();String				ls_mes_periodo
Integer				columna, li_fila
Str_busqueda		lstr_busq

il_fila	=	dw_1.GetRow()

CHOOSE CASE gs_windows
	CASE '2'
			dw_1.AcceptText()
			/*acá se asignan los valores a traspasar a la ventana emergente*/
			lstr_busq.argum[1]	=	String(uo_muestranave.dw_seleccion.Object.Codigo[1])
			lstr_busq.argum[2]	=	uo_muestratipo.dw_seleccion.Object.Codigo[1]
			lstr_busq.argum[3]   =  em_fecha.text
			lstr_busq.argum[4]   =  String(uo_muestradest.dw_seleccion.Object.Codigo[1])
			lstr_busq.argum[5]   =  String(dw_1.Object.reci_codigo[il_fila])
			/*Se habre la ventana con los parametros correspondiente*/
			OpenSheetWithParm(w_cons_listado_porplanillas, lstr_busq, w_main, columna, Original!)

END CHOOSE

	
end event

on w_cons_listado_barcos.create
this.uo_muestradest=create uo_muestradest
this.em_fecha=create em_fecha
this.st_3=create st_3
this.uo_muestratipo=create uo_muestratipo
this.st_5=create st_5
this.uo_muestranave=create uo_muestranave
this.st_6=create st_6
this.pb_imprimir=create pb_imprimir
this.pb_2=create pb_2
this.st_2=create st_2
this.dw_1=create dw_1
this.pb_salir=create pb_salir
this.pb_1=create pb_1
this.gb_4=create gb_4
this.gb_5=create gb_5
this.st_1=create st_1
this.Control[]={this.uo_muestradest,&
this.em_fecha,&
this.st_3,&
this.uo_muestratipo,&
this.st_5,&
this.uo_muestranave,&
this.st_6,&
this.pb_imprimir,&
this.pb_2,&
this.st_2,&
this.dw_1,&
this.pb_salir,&
this.pb_1,&
this.gb_4,&
this.gb_5,&
this.st_1}
end on

on w_cons_listado_barcos.destroy
destroy(this.uo_muestradest)
destroy(this.em_fecha)
destroy(this.st_3)
destroy(this.uo_muestratipo)
destroy(this.st_5)
destroy(this.uo_muestranave)
destroy(this.st_6)
destroy(this.pb_imprimir)
destroy(this.pb_2)
destroy(this.st_2)
destroy(this.dw_1)
destroy(this.pb_salir)
destroy(this.pb_1)
destroy(this.gb_4)
destroy(this.gb_5)
destroy(this.st_1)
end on

event open;im_menu		= m_principal
Boolean	lb_Cerrar

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
							
istr_busq	= Message.PowerObjectParm								
							
IF IsNull(uo_muestratipo.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestranave.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestradest.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_muestratipo.Seleccion(True, True)
	uo_muestranave.Seleccion(True, True)
	uo_muestradest.Seleccion(True, True)
	
	//Tipo Transporte
	uo_muestratipo.cbx_todos.checked     = FALSE
	uo_muestratipo.cbx_todos.visible     = FALSE
	uo_muestratipo.cbx_consolida.visible = FALSE
	uo_muestratipo.dw_seleccion.enabled  = FALSE
	uo_muestratipo.dw_seleccion.Object.Codigo[1] = istr_busq.argum[1]
	
	//Destino
	uo_muestradest.cbx_todos.checked     = FALSE
	uo_muestradest.cbx_todos.visible     = FALSE
	uo_muestradest.cbx_consolida.visible = FALSE
	uo_muestradest.dw_seleccion.enabled  = FALSE
	uo_muestradest.dw_seleccion.Object.Codigo[1] = Integer(istr_busq.argum[2])
		
	//Nave
	uo_muestranave.cbx_todos.checked     = FALSE
	uo_muestranave.cbx_todos.visible     = FALSE
	uo_muestranave.cbx_consolida.visible = FALSE
	uo_muestranave.dw_seleccion.enabled  = FALSE
	uo_muestranave.filtra('*')	
	uo_muestranave.dw_seleccion.Object.Codigo[1] = Integer(istr_busq.argum[3])
END IF

em_fecha.text = istr_busq.argum[4]
pb_2.TriggerEvent(Clicked!)
							

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

event resize;x=0	
y=0
end event

type uo_muestradest from uo_seleccion_destinos_mod within w_cons_listado_barcos
integer x = 2094
integer y = 196
integer width = 882
integer height = 116
integer taborder = 40
end type

on uo_muestradest.destroy
call uo_seleccion_destinos_mod::destroy
end on

type em_fecha from editmask within w_cons_listado_barcos
integer x = 2098
integer y = 104
integer width = 343
integer height = 84
integer taborder = 30
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_3 from statictext within w_cons_listado_barcos
integer x = 1673
integer y = 112
integer width = 407
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Fecha de Arribo"
boolean focusrectangle = false
end type

type uo_muestratipo from uo_seleccion_tipotransporte_mod within w_cons_listado_barcos
integer x = 699
integer y = 80
integer width = 896
integer taborder = 20
end type

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE '*'
		uo_muestranave.Todos(True)
		uo_muestranave.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		uo_muestranave.Filtra(This.Codigo)
		uo_muestranave.cbx_Todos.Enabled	=	True
END CHOOSE
end event

on uo_muestratipo.destroy
call uo_seleccion_tipotransporte_mod::destroy
end on

type st_5 from statictext within w_cons_listado_barcos
integer x = 238
integer y = 112
integer width = 416
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Tipo Transporte"
boolean focusrectangle = false
end type

type uo_muestranave from uo_seleccion_naves_mod within w_cons_listado_barcos
integer x = 699
integer y = 196
integer width = 901
integer taborder = 30
end type

on uo_muestranave.destroy
call uo_seleccion_naves_mod::destroy
end on

type st_6 from statictext within w_cons_listado_barcos
integer x = 1673
integer y = 208
integer width = 361
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Pais Destino"
boolean focusrectangle = false
end type

type pb_imprimir from picturebutton within w_cons_listado_barcos
string tag = "Selección de Parámetros"
integer x = 3259
integer y = 728
integer width = 155
integer height = 132
integer taborder = 70
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\desarrollo\bmp\imprimee.bmp"
string disabledname = "\desarrollo\bmp\imprimed.bmp"
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
//ls_Mascara			=	F_Global_Replace(gstr_param.mas_cuenta, "X", "@")
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
//	IF gs_Ambiente <> 'Windows' THEN
//		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
//
//	END IF
//END IF
//
//SetPointer(Arrow!)
end event

type pb_2 from picturebutton within w_cons_listado_barcos
string tag = "Selección de Parámetros"
integer x = 3264
integer y = 524
integer width = 155
integer height = 132
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\rescatae.bmp"
string disabledname = "\desarrollo\bmp\rescatadd.bmp"
alignment htextalign = left!
end type

event clicked;Date	ld_Fecha
Integer li_nave, li_puerto, li_destino
String ls_tipo

ld_fecha = date(em_fecha.text)

li_nave = uo_muestranave.dw_seleccion.Object.codigo[1]
IF IsNull(li_nave) THEN
	MessageBox("Atención","Debe Seleccionar una nave Previamente",Exclamation!)
	RETURN
END IF

li_destino = uo_muestradest.dw_seleccion.Object.codigo[1]
IF IsNull(li_destino) THEN
	MessageBox("Atención","Debe Seleccionar un destino Previamente",Exclamation!)
	RETURN
END IF

ls_tipo = uo_muestratipo.dw_seleccion.Object.codigo[1]
IF IsNull(ls_tipo) THEN
	MessageBox("Atención","Debe Seleccionar un Tipo de Transporte Previamente",Exclamation!)
	RETURN
END IF

dw_1.Retrieve(li_nave,ls_tipo,li_destino,ld_fecha)
	
dw_1.SetRowFocusIndicator(Hand!)
dw_1.SetFocus()
		
IF dw_1.RowCount() = 0 THEN
	MessageBox('ADVERTENCIA','No Existe información')
	RETURN
ELSE
	pb_imprimir.Enabled	=	True
END IF
end event

type st_2 from statictext within w_cons_listado_barcos
integer x = 233
integer y = 224
integer width = 270
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Nave"
boolean focusrectangle = false
end type

type dw_1 from uo_dw within w_cons_listado_barcos
integer x = 37
integer y = 396
integer width = 3067
integer height = 1400
integer taborder = 0
boolean titlebar = true
string title = " Detalle Nave"
string dataobject = "dw_cons_listado_barcos"
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
	
	 m_consulta.m_m_edicion.m_listadoporplanillas.Visible	 					=	True
	 m_consulta.m_m_edicion.m_listadopornave.Visible	 					   =	False
	 m_consulta.m_m_edicion.m_detalleplanilla.Visible					      =	False

	l_Menu.m_m_edicion.PopMenu(This.PointerX(),This.PointerY()+750)
	
END IF

end event

event doubleclicked;IF Row <> 0 THEN
	gs_windows		=	'1'
	gs_opcion		=  '2'
	Parent.TriggerEvent('ue_asignacion')
	This.SetFocus()
END IF
end event

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

type pb_salir from picturebutton within w_cons_listado_barcos
integer x = 3259
integer y = 1516
integer width = 155
integer height = 132
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\desarrollo\bmp\exit.bmp"
alignment htextalign = left!
end type

event clicked;call super::clicked;Close(parent)
end event

type pb_1 from picturebutton within w_cons_listado_barcos
string tag = "Selección de Parámetros"
integer x = 3259
integer y = 1312
integer width = 155
integer height = 132
integer taborder = 80
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\nuevoe.bmp"
string disabledname = "\desarrollo\bmp\nuevod.bmp"
alignment htextalign = left!
end type

type gb_4 from groupbox within w_cons_listado_barcos
integer x = 3200
integer y = 1220
integer width = 274
integer height = 504
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_5 from groupbox within w_cons_listado_barcos
integer x = 3200
integer y = 428
integer width = 274
integer height = 504
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type st_1 from statictext within w_cons_listado_barcos
integer x = 128
integer y = 60
integer width = 2903
integer height = 272
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

