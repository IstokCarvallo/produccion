$PBExportHeader$w_cons_listado_general.srw
forward
global type w_cons_listado_general from window
end type
type uo_muestradest from uo_seleccion_destinos_mod within w_cons_listado_general
end type
type st_3 from statictext within w_cons_listado_general
end type
type st_2 from statictext within w_cons_listado_general
end type
type uo_muestratipo from uo_seleccion_tipotransporte_mod within w_cons_listado_general
end type
type st_5 from statictext within w_cons_listado_general
end type
type st_6 from statictext within w_cons_listado_general
end type
type pb_imprimir from picturebutton within w_cons_listado_general
end type
type pb_2 from picturebutton within w_cons_listado_general
end type
type dw_1 from uo_dw within w_cons_listado_general
end type
type pb_salir from picturebutton within w_cons_listado_general
end type
type pb_1 from picturebutton within w_cons_listado_general
end type
type gb_4 from groupbox within w_cons_listado_general
end type
type gb_5 from groupbox within w_cons_listado_general
end type
type uo_muestraespecie from uo_seleccion_especie_mod within w_cons_listado_general
end type
type st_1 from statictext within w_cons_listado_general
end type
end forward

global type w_cons_listado_general from window
integer x = 5
integer y = 16
integer width = 4119
integer height = 2020
boolean titlebar = true
string title = "Consulta listado General"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 30586022
event ue_asignacion ( )
uo_muestradest uo_muestradest
st_3 st_3
st_2 st_2
uo_muestratipo uo_muestratipo
st_5 st_5
st_6 st_6
pb_imprimir pb_imprimir
pb_2 pb_2
dw_1 dw_1
pb_salir pb_salir
pb_1 pb_1
gb_4 gb_4
gb_5 gb_5
uo_muestraespecie uo_muestraespecie
st_1 st_1
end type
global w_cons_listado_general w_cons_listado_general

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
			lstr_busq.argum[1]	=	uo_muestratipo.dw_seleccion.Object.Codigo[1]
			lstr_busq.argum[2]	=	String(uo_muestradest.dw_seleccion.Object.Codigo[1])
			lstr_busq.argum[3]   =  String(dw_1.Object.nave_codigo[il_fila])
			lstr_busq.argum[4]   =  String(dw_1.Object.cpde_fecarr[il_fila])
			/*Se habre la ventana con los parametros correspondiente*/
			OpenSheetWithParm(w_cons_listado_barcos, lstr_busq, w_main, columna, Original!)
	
END CHOOSE

	
end event

on w_cons_listado_general.create
this.uo_muestradest=create uo_muestradest
this.st_3=create st_3
this.st_2=create st_2
this.uo_muestratipo=create uo_muestratipo
this.st_5=create st_5
this.st_6=create st_6
this.pb_imprimir=create pb_imprimir
this.pb_2=create pb_2
this.dw_1=create dw_1
this.pb_salir=create pb_salir
this.pb_1=create pb_1
this.gb_4=create gb_4
this.gb_5=create gb_5
this.uo_muestraespecie=create uo_muestraespecie
this.st_1=create st_1
this.Control[]={this.uo_muestradest,&
this.st_3,&
this.st_2,&
this.uo_muestratipo,&
this.st_5,&
this.st_6,&
this.pb_imprimir,&
this.pb_2,&
this.dw_1,&
this.pb_salir,&
this.pb_1,&
this.gb_4,&
this.gb_5,&
this.uo_muestraespecie,&
this.st_1}
end on

on w_cons_listado_general.destroy
destroy(this.uo_muestradest)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.uo_muestratipo)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.pb_imprimir)
destroy(this.pb_2)
destroy(this.dw_1)
destroy(this.pb_salir)
destroy(this.pb_1)
destroy(this.gb_4)
destroy(this.gb_5)
destroy(this.uo_muestraespecie)
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
							
							
IF IsNull(uo_muestratipo.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestradest.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestraespecie.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_muestratipo.Seleccion(True, True)
	uo_muestradest.Seleccion(True, True)
	uo_muestraespecie.Seleccion(True, True)

	//Tipo Transporte
	uo_muestratipo.cbx_todos.checked     = FALSE
	uo_muestratipo.cbx_todos.visible     = FALSE
	uo_muestratipo.cbx_consolida.visible = FALSE
	uo_muestratipo.dw_seleccion.enabled  = TRUE
	uo_muestratipo.dw_seleccion.SetFocus()
	
	//Destino
	uo_muestradest.cbx_todos.checked     = FALSE
	uo_muestradest.cbx_todos.visible     = FALSE
	uo_muestradest.cbx_consolida.visible = FALSE
	uo_muestradest.dw_seleccion.enabled  = TRUE
	
	// Especie
	uo_muestraespecie.cbx_todos.checked     = TRUE
	uo_muestraespecie.cbx_todos.visible     = TRUE
	uo_muestraespecie.cbx_consolida.visible = FALSE
	uo_muestraespecie.dw_seleccion.enabled  = TRUE
		
END IF
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

type uo_muestradest from uo_seleccion_destinos_mod within w_cons_listado_general
event destroy ( )
integer x = 2322
integer y = 104
integer width = 905
integer taborder = 30
end type

on uo_muestradest.destroy
call uo_seleccion_destinos_mod::destroy
end on

type st_3 from statictext within w_cons_listado_general
integer x = 2007
integer y = 244
integer width = 201
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Todas"
boolean focusrectangle = false
end type

type st_2 from statictext within w_cons_listado_general
integer x = 544
integer y = 236
integer width = 416
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type uo_muestratipo from uo_seleccion_tipotransporte_mod within w_cons_listado_general
integer x = 1006
integer y = 108
integer width = 896
integer taborder = 20
end type

on uo_muestratipo.destroy
call uo_seleccion_tipotransporte_mod::destroy
end on

type st_5 from statictext within w_cons_listado_general
integer x = 544
integer y = 140
integer width = 416
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
boolean enabled = false
string text = "Tipo Transporte"
boolean focusrectangle = false
end type

type st_6 from statictext within w_cons_listado_general
integer x = 1979
integer y = 140
integer width = 343
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
boolean enabled = false
string text = "Pais Destino"
boolean focusrectangle = false
end type

type pb_imprimir from picturebutton within w_cons_listado_general
string tag = "Selección de Parámetros"
integer x = 3762
integer y = 816
integer width = 233
integer height = 196
integer taborder = 70
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 12\Imagenes\Botones\ImprimirEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\ImprimirDisab.png"
alignment htextalign = left!
long backcolor = 33543637
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

type pb_2 from picturebutton within w_cons_listado_general
string tag = "Selección de Parámetros"
integer x = 3767
integer y = 612
integer width = 233
integer height = 196
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 12\Imagenes\Botones\BuscarEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\BuscarDisab.png"
alignment htextalign = left!
long backcolor = 33543637
end type

event clicked;String ls_tipo
Integer li_especie, li_destino

ls_tipo = uo_muestratipo.dw_seleccion.Object.codigo[1]
IF IsNull(ls_tipo) THEN
	MessageBox("Atención","Debe Seleccionar un Tipo de Transporte Previamente",Exclamation!)
	RETURN
END IF

IF uo_muestraespecie.cbx_todos.checked THEN
   li_especie	= -1
ELSE
	li_especie = uo_muestraespecie.dw_seleccion.Object.codigo[1]
	IF IsNull(li_especie) THEN
		MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
		RETURN
	END IF
END IF

li_destino = uo_muestradest.dw_seleccion.Object.codigo[1]
IF IsNull(li_destino) THEN
	MessageBox("Atención","Debe Seleccionar un destino Previamente",Exclamation!)
	RETURN
END IF

dw_1.Retrieve(ls_tipo,li_destino,li_especie)
	
dw_1.SetRowFocusIndicator(Hand!)
dw_1.SetFocus()
		
IF dw_1.RowCount() = 0 THEN
	MessageBox('ADVERTENCIA','No Existe información')
	RETURN
ELSE
	pb_imprimir.Enabled	=	True
END IF
end event

type dw_1 from uo_dw within w_cons_listado_general
integer x = 37
integer y = 424
integer width = 3643
integer height = 1400
integer taborder = 0
boolean titlebar = true
string title = " Detalle General"
string dataobject = "dw_cons_listado_general"
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
	
	 m_consulta.m_m_edicion.m_listadoporplanillas.Visible	 					=	False
	 m_consulta.m_m_edicion.m_listadopornave.Visible	 				      =	True
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

type pb_salir from picturebutton within w_cons_listado_general
integer x = 3762
integer y = 1604
integer width = 233
integer height = 196
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 12\Imagenes\Botones\SalirEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\SalirDisab.png"
alignment htextalign = left!
long backcolor = 33543637
end type

event clicked;call super::clicked;Close(parent)
end event

type pb_1 from picturebutton within w_cons_listado_general
string tag = "Selección de Parámetros"
integer x = 3753
integer y = 1396
integer width = 233
integer height = 196
integer taborder = 80
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 12\Imagenes\Botones\NuevoEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\NuevoDisab.png"
alignment htextalign = left!
long backcolor = 33543637
end type

type gb_4 from groupbox within w_cons_listado_general
integer x = 3735
integer y = 1316
integer width = 274
integer height = 504
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type gb_5 from groupbox within w_cons_listado_general
integer x = 3739
integer y = 540
integer width = 274
integer height = 504
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type uo_muestraespecie from uo_seleccion_especie_mod within w_cons_listado_general
integer x = 1006
integer y = 212
integer width = 1038
integer height = 124
integer taborder = 60
end type

on uo_muestraespecie.destroy
call uo_seleccion_especie_mod::destroy
end on

type st_1 from statictext within w_cons_listado_general
integer x = 434
integer y = 72
integer width = 2903
integer height = 324
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

