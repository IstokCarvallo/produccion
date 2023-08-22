$PBExportHeader$w_cons_informe_llegadadestino.srw
forward
global type w_cons_informe_llegadadestino from window
end type
type st_24 from statictext within w_cons_informe_llegadadestino
end type
type st_23 from statictext within w_cons_informe_llegadadestino
end type
type st_22 from statictext within w_cons_informe_llegadadestino
end type
type st_21 from statictext within w_cons_informe_llegadadestino
end type
type st_20 from statictext within w_cons_informe_llegadadestino
end type
type st_19 from statictext within w_cons_informe_llegadadestino
end type
type st_18 from statictext within w_cons_informe_llegadadestino
end type
type st_17 from statictext within w_cons_informe_llegadadestino
end type
type st_16 from statictext within w_cons_informe_llegadadestino
end type
type st_15 from statictext within w_cons_informe_llegadadestino
end type
type st_10 from statictext within w_cons_informe_llegadadestino
end type
type cbx_todosgua from checkbox within w_cons_informe_llegadadestino
end type
type cbx_todoscon from checkbox within w_cons_informe_llegadadestino
end type
type cbx_todoscal from checkbox within w_cons_informe_llegadadestino
end type
type cbx_todosfecha from checkbox within w_cons_informe_llegadadestino
end type
type ddlb_condicion from dropdownlistbox within w_cons_informe_llegadadestino
end type
type ddlb_guarda from dropdownlistbox within w_cons_informe_llegadadestino
end type
type ddlb_calidad from dropdownlistbox within w_cons_informe_llegadadestino
end type
type st_9 from statictext within w_cons_informe_llegadadestino
end type
type uo_muestraespecies from uo_seleccion_especie_mod within w_cons_informe_llegadadestino
end type
type em_hasta from editmask within w_cons_informe_llegadadestino
end type
type em_desde from editmask within w_cons_informe_llegadadestino
end type
type st_14 from statictext within w_cons_informe_llegadadestino
end type
type st_13 from statictext within w_cons_informe_llegadadestino
end type
type st_12 from statictext within w_cons_informe_llegadadestino
end type
type st_11 from statictext within w_cons_informe_llegadadestino
end type
type uo_muestraprod from uo_seleccion_productor_mod within w_cons_informe_llegadadestino
end type
type uo_muestrareci from uo_seleccion_recibidor_planilla_mod within w_cons_informe_llegadadestino
end type
type uo_muestramercado from uo_seleccion_mercado_mod within w_cons_informe_llegadadestino
end type
type uo_muestravariedad from uo_seleccion_variedad_mod within w_cons_informe_llegadadestino
end type
type uo_muestrazona from uo_seleccion_zonas_mod within w_cons_informe_llegadadestino
end type
type st_7 from statictext within w_cons_informe_llegadadestino
end type
type st_4 from statictext within w_cons_informe_llegadadestino
end type
type st_3 from statictext within w_cons_informe_llegadadestino
end type
type st_5 from statictext within w_cons_informe_llegadadestino
end type
type uo_muestranave from uo_seleccion_naves_mod within w_cons_informe_llegadadestino
end type
type st_6 from statictext within w_cons_informe_llegadadestino
end type
type pb_imprimir from picturebutton within w_cons_informe_llegadadestino
end type
type pb_buscar from picturebutton within w_cons_informe_llegadadestino
end type
type st_2 from statictext within w_cons_informe_llegadadestino
end type
type dw_1 from uo_dw within w_cons_informe_llegadadestino
end type
type pb_salir from picturebutton within w_cons_informe_llegadadestino
end type
type pb_nuevo from picturebutton within w_cons_informe_llegadadestino
end type
type gb_4 from groupbox within w_cons_informe_llegadadestino
end type
type gb_5 from groupbox within w_cons_informe_llegadadestino
end type
type gb_2 from groupbox within w_cons_informe_llegadadestino
end type
type gb_1 from groupbox within w_cons_informe_llegadadestino
end type
type st_1 from statictext within w_cons_informe_llegadadestino
end type
end forward

global type w_cons_informe_llegadadestino from window
integer x = 5
integer y = 16
integer width = 5038
integer height = 2316
boolean titlebar = true
string title = "Consulta Llegada a destino por Productor"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 553648127
event ue_asignacion ( )
st_24 st_24
st_23 st_23
st_22 st_22
st_21 st_21
st_20 st_20
st_19 st_19
st_18 st_18
st_17 st_17
st_16 st_16
st_15 st_15
st_10 st_10
cbx_todosgua cbx_todosgua
cbx_todoscon cbx_todoscon
cbx_todoscal cbx_todoscal
cbx_todosfecha cbx_todosfecha
ddlb_condicion ddlb_condicion
ddlb_guarda ddlb_guarda
ddlb_calidad ddlb_calidad
st_9 st_9
uo_muestraespecies uo_muestraespecies
em_hasta em_hasta
em_desde em_desde
st_14 st_14
st_13 st_13
st_12 st_12
st_11 st_11
uo_muestraprod uo_muestraprod
uo_muestrareci uo_muestrareci
uo_muestramercado uo_muestramercado
uo_muestravariedad uo_muestravariedad
uo_muestrazona uo_muestrazona
st_7 st_7
st_4 st_4
st_3 st_3
st_5 st_5
uo_muestranave uo_muestranave
st_6 st_6
pb_imprimir pb_imprimir
pb_buscar pb_buscar
st_2 st_2
dw_1 dw_1
pb_salir pb_salir
pb_nuevo pb_nuevo
gb_4 gb_4
gb_5 gb_5
gb_2 gb_2
gb_1 gb_1
st_1 st_1
end type
global w_cons_informe_llegadadestino w_cons_informe_llegadadestino

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
			lstr_busq.argum[1]	=	String(dw_1.Object.cpde_numero[il_fila])
			lstr_busq.Argum[2]   =  String(dw_1.Object.espe_codigo[il_fila])
			lstr_busq.Argum[3]   =  String(dw_1.Object.clie_codigo[il_fila])
			lstr_busq.Argum[4]   =  'consulta'
			lstr_busq.Argum[5]   =  String(dw_1.Object.merc_codigo[il_fila])
			lstr_busq.Argum[6]   =  String(dw_1.Object.dest_codigo[il_fila])
			/*Se habre la ventana con los parametros correspondiente*/
			OpenSheetWithParm(w_maed_planillainspecdestino, lstr_busq, w_main, columna, Original!)
END CHOOSE

	
end event

on w_cons_informe_llegadadestino.create
this.st_24=create st_24
this.st_23=create st_23
this.st_22=create st_22
this.st_21=create st_21
this.st_20=create st_20
this.st_19=create st_19
this.st_18=create st_18
this.st_17=create st_17
this.st_16=create st_16
this.st_15=create st_15
this.st_10=create st_10
this.cbx_todosgua=create cbx_todosgua
this.cbx_todoscon=create cbx_todoscon
this.cbx_todoscal=create cbx_todoscal
this.cbx_todosfecha=create cbx_todosfecha
this.ddlb_condicion=create ddlb_condicion
this.ddlb_guarda=create ddlb_guarda
this.ddlb_calidad=create ddlb_calidad
this.st_9=create st_9
this.uo_muestraespecies=create uo_muestraespecies
this.em_hasta=create em_hasta
this.em_desde=create em_desde
this.st_14=create st_14
this.st_13=create st_13
this.st_12=create st_12
this.st_11=create st_11
this.uo_muestraprod=create uo_muestraprod
this.uo_muestrareci=create uo_muestrareci
this.uo_muestramercado=create uo_muestramercado
this.uo_muestravariedad=create uo_muestravariedad
this.uo_muestrazona=create uo_muestrazona
this.st_7=create st_7
this.st_4=create st_4
this.st_3=create st_3
this.st_5=create st_5
this.uo_muestranave=create uo_muestranave
this.st_6=create st_6
this.pb_imprimir=create pb_imprimir
this.pb_buscar=create pb_buscar
this.st_2=create st_2
this.dw_1=create dw_1
this.pb_salir=create pb_salir
this.pb_nuevo=create pb_nuevo
this.gb_4=create gb_4
this.gb_5=create gb_5
this.gb_2=create gb_2
this.gb_1=create gb_1
this.st_1=create st_1
this.Control[]={this.st_24,&
this.st_23,&
this.st_22,&
this.st_21,&
this.st_20,&
this.st_19,&
this.st_18,&
this.st_17,&
this.st_16,&
this.st_15,&
this.st_10,&
this.cbx_todosgua,&
this.cbx_todoscon,&
this.cbx_todoscal,&
this.cbx_todosfecha,&
this.ddlb_condicion,&
this.ddlb_guarda,&
this.ddlb_calidad,&
this.st_9,&
this.uo_muestraespecies,&
this.em_hasta,&
this.em_desde,&
this.st_14,&
this.st_13,&
this.st_12,&
this.st_11,&
this.uo_muestraprod,&
this.uo_muestrareci,&
this.uo_muestramercado,&
this.uo_muestravariedad,&
this.uo_muestrazona,&
this.st_7,&
this.st_4,&
this.st_3,&
this.st_5,&
this.uo_muestranave,&
this.st_6,&
this.pb_imprimir,&
this.pb_buscar,&
this.st_2,&
this.dw_1,&
this.pb_salir,&
this.pb_nuevo,&
this.gb_4,&
this.gb_5,&
this.gb_2,&
this.gb_1,&
this.st_1}
end on

on w_cons_informe_llegadadestino.destroy
destroy(this.st_24)
destroy(this.st_23)
destroy(this.st_22)
destroy(this.st_21)
destroy(this.st_20)
destroy(this.st_19)
destroy(this.st_18)
destroy(this.st_17)
destroy(this.st_16)
destroy(this.st_15)
destroy(this.st_10)
destroy(this.cbx_todosgua)
destroy(this.cbx_todoscon)
destroy(this.cbx_todoscal)
destroy(this.cbx_todosfecha)
destroy(this.ddlb_condicion)
destroy(this.ddlb_guarda)
destroy(this.ddlb_calidad)
destroy(this.st_9)
destroy(this.uo_muestraespecies)
destroy(this.em_hasta)
destroy(this.em_desde)
destroy(this.st_14)
destroy(this.st_13)
destroy(this.st_12)
destroy(this.st_11)
destroy(this.uo_muestraprod)
destroy(this.uo_muestrareci)
destroy(this.uo_muestramercado)
destroy(this.uo_muestravariedad)
destroy(this.uo_muestrazona)
destroy(this.st_7)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.uo_muestranave)
destroy(this.st_6)
destroy(this.pb_imprimir)
destroy(this.pb_buscar)
destroy(this.st_2)
destroy(this.dw_1)
destroy(this.pb_salir)
destroy(this.pb_nuevo)
destroy(this.gb_4)
destroy(this.gb_5)
destroy(this.gb_2)
destroy(this.gb_1)
destroy(this.st_1)
end on

event open;/*
   istr_mant.argumento[1]  = Calificación calidad
	istr_mant.argumento[2]  = Calificación condición
	istr_mant.argumento[3]  = Guarda Caja
	istr_mant.argumento[4]  = Especie
	istr_mant.argumento[5]  = Variedad
	istr_mant.argumento[6]  = Zona
	istr_mant.argumento[7]  = Mercado	
	istr_mant.argumento[8]  = Productor
	istr_mant.argumento[9]  = Nave
	istr_mant.argumento[10] = Recibidor
	istr_mant.argumento[11] = Fecha Desde
	istr_mant.argumento[12] = Fecha Hasta
*/
im_menu		= m_principal
Boolean	lb_Cerrar

//This.Icon									=	Gstr_apl.Icono
//This.ParentWindow().ToolBarVisible	=	True
//im_menu.Item[1].Item[6].Enabled		=	True
//im_menu.Item[7].Visible					=	True

//This.ParentWindow().WindowState		=	Maximized!
//This.WindowState							=	Maximized!

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
							
IF IsNull(uo_muestraespecies.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestravariedad.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestrazona.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestramercado.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestraprod.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestranave.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_muestrareci.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
   uo_muestraespecies.Seleccion(True, True)
	uo_muestravariedad.Seleccion(True, True)
	uo_muestrazona.Seleccion(True, True)
	uo_muestramercado.Seleccion(True, True)
	uo_muestraprod.Seleccion(True, True)
	uo_muestranave.Seleccion(True, True)
	uo_muestrareci.Seleccion(True, True)
	
	//Especie
	uo_muestraespecies.cbx_todos.checked     = FALSE
	uo_muestraespecies.cbx_todos.visible     = TRUE
	uo_muestraespecies.cbx_consolida.visible = TRUE
	uo_muestraespecies.dw_seleccion.enabled  = TRUE
	uo_muestraespecies.dw_seleccion.object.codigo[1] = 11
			
	//Variedad
	uo_muestravariedad.cbx_todos.checked     = TRUE
	uo_muestravariedad.cbx_consolida.checked = FALSE
	uo_muestravariedad.dw_seleccion.enabled  = TRUE
	uo_muestravariedad.Filtra(0)
	
	//Zona
	uo_muestrazona.cbx_todos.checked     = TRUE
	uo_muestrazona.cbx_consolida.checked = FALSE
	uo_muestrazona.dw_seleccion.enabled  = TRUE
	
	//Mercado
	uo_muestramercado.cbx_todos.checked     = TRUE
	uo_muestramercado.cbx_consolida.checked = FALSE
	uo_muestramercado.dw_seleccion.enabled  = TRUE
	
	//Productor
	uo_muestraprod.cbx_todos.checked     = TRUE
	uo_muestraprod.cbx_consolida.checked = FALSE
	uo_muestraprod.dw_seleccion.enabled  = TRUE
	uo_muestraprod.Filtra(0)
	
	//Nave
	uo_muestranave.cbx_todos.checked     = TRUE
	uo_muestranave.cbx_consolida.checked = FALSE
	uo_muestranave.dw_seleccion.enabled  = TRUE
	//uo_muestranave.dw_seleccion.object.codigo[1] = 11 
	
	//Recibidor
	uo_muestrareci.cbx_todos.checked     = TRUE
	uo_muestrareci.cbx_consolida.checked = FALSE
	uo_muestrareci.dw_seleccion.enabled  = TRUE
	uo_muestrareci.Filtra(0)
	
	em_desde.text	  = String(Today())
	em_hasta.text	  = String(Today())
	
	istr_mant.argumento[1]  = '-1'
	istr_mant.argumento[2]  = '-1'
	istr_mant.argumento[3]  = '-1'
	istr_mant.argumento[4]  = '-1'
	istr_mant.argumento[5]  = '-1'
	istr_mant.argumento[6]  = '-1'
	istr_mant.argumento[7]  = '-1'
	istr_mant.argumento[8]  = '-1'
	istr_mant.argumento[9]  = '-1'
	istr_mant.argumento[10] = '-1'
	istr_mant.argumento[11] = '-1'
	istr_mant.argumento[12] = '-1'
	em_desde.Enabled = FALSE
	em_hasta.Enabled = FALSE
	ddlb_calidad.Enabled = FALSE
	ddlb_condicion.Enabled = FALSE
	ddlb_guarda.Enabled = FALSE
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

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

	dw_1.width	=	This.WorkSpaceWidth() - 400
	maximo		=	dw_1.width

dw_1.x					= 37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.height				= This.WorkSpaceHeight() - dw_1.y - 41

gb_4.x 					= This.WorkSpaceWidth() - 310
gb_4.y 					= 5
gb_4.width				= 275

li_posic_x				= This.WorkSpaceWidth() - 250
li_posic_y				= gb_4.y + 88

IF pb_buscar.Visible THEN
	pb_buscar.x			= li_posic_x
	pb_buscar.y			= li_posic_y
	pb_buscar.width		= 235
	pb_buscar.height		= 195
	li_visible ++
	li_posic_y += 195
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				= li_posic_x
	pb_nuevo.y				= li_posic_y
	pb_nuevo.width		= 235
	pb_nuevo.height		= 195
	li_visible ++
	li_posic_y += 195
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			= li_posic_x
	pb_imprimir.y			= li_posic_y
	pb_imprimir.width		= 235
	pb_imprimir.height		= 195
	li_visible ++
	li_posic_y += 195
END IF

IF pb_salir.Visible THEN
	pb_salir.x				= li_posic_x
	pb_salir.y				= li_posic_y
	pb_salir.width			= 235
	pb_salir.height			= 195
	li_visible ++
	li_posic_y += 195
END IF

gb_4.height				= 180 * li_visible + 97 /*  (Según Botones Visibles)  */
gb_5.x 					= gb_1.x
gb_5.y 					= 1293
gb_5.width				= 275
gb_5.height				= 180 * 2 + 97 /*  (2 Botones)  */


end event

type st_24 from statictext within w_cons_informe_llegadadestino
integer x = 2002
integer y = 616
integer width = 201
integer height = 56
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

type st_23 from statictext within w_cons_informe_llegadadestino
integer x = 3026
integer y = 664
integer width = 201
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Todos"
boolean focusrectangle = false
end type

type st_22 from statictext within w_cons_informe_llegadadestino
integer x = 3026
integer y = 560
integer width = 201
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Todos"
boolean focusrectangle = false
end type

type st_21 from statictext within w_cons_informe_llegadadestino
integer x = 3931
integer y = 564
integer width = 201
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Todos"
boolean focusrectangle = false
end type

type st_20 from statictext within w_cons_informe_llegadadestino
integer x = 3931
integer y = 264
integer width = 201
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Todos"
boolean focusrectangle = false
end type

type st_19 from statictext within w_cons_informe_llegadadestino
integer x = 3931
integer y = 164
integer width = 201
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Todos"
boolean focusrectangle = false
end type

type st_18 from statictext within w_cons_informe_llegadadestino
integer x = 3931
integer y = 68
integer width = 201
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Todos"
boolean focusrectangle = false
end type

type st_17 from statictext within w_cons_informe_llegadadestino
integer x = 2149
integer y = 384
integer width = 210
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Todos"
boolean focusrectangle = false
end type

type st_16 from statictext within w_cons_informe_llegadadestino
integer x = 2149
integer y = 276
integer width = 210
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Todos"
boolean focusrectangle = false
end type

type st_15 from statictext within w_cons_informe_llegadadestino
integer x = 2149
integer y = 172
integer width = 210
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Todos"
boolean focusrectangle = false
end type

type st_10 from statictext within w_cons_informe_llegadadestino
integer x = 2149
integer y = 68
integer width = 210
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Todos"
boolean focusrectangle = false
end type

type cbx_todosgua from checkbox within w_cons_informe_llegadadestino
integer x = 3845
integer y = 564
integer width = 73
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 33543637
boolean checked = true
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[3] = "-1"
	ddlb_guarda.Enabled = False
ELSE
	istr_mant.argumento[3] = ""
	ddlb_guarda.Enabled = True
END IF
end event

type cbx_todoscon from checkbox within w_cons_informe_llegadadestino
integer x = 2944
integer y = 660
integer width = 82
integer height = 72
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
boolean checked = true
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[2] = "-1"
	ddlb_condicion.Enabled = False
ELSE
	istr_mant.argumento[2] = ""
	ddlb_condicion.Enabled = True
END IF
end event

type cbx_todoscal from checkbox within w_cons_informe_llegadadestino
integer x = 2939
integer y = 556
integer width = 96
integer height = 72
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
boolean checked = true
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[1] = "-1"
	ddlb_calidad.Enabled = False
ELSE
	istr_mant.argumento[1] = ""
	ddlb_calidad.Enabled = True
END IF
end event

type cbx_todosfecha from checkbox within w_cons_informe_llegadadestino
integer x = 1911
integer y = 612
integer width = 101
integer height = 72
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_desde.Enabled	=	FALSE
	em_hasta.Enabled	=	FALSE
   istr_Mant.Argumento[11]	=	'01/01/1900'
	istr_Mant.Argumento[12]	=	String(Today())
ELSE
	em_desde.Enabled	=	TRUE
	em_hasta.Enabled	=	TRUE
	em_desde.SetFocus()
END IF
RETURN 0

end event

type ddlb_condicion from dropdownlistbox within w_cons_informe_llegadadestino
integer x = 2537
integer y = 652
integer width = 379
integer height = 376
integer taborder = 100
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean sorted = false
string item[] = {"Bueno","Regular","Malo"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF index = 1 THEN
	istr_mant.argumento[2] = '1'
ELSEIF index = 2 THEN
	istr_mant.argumento[2] = '2'
ELSE
	istr_mant.argumento[2] = '3'
END IF
cbx_todoscon.Checked = FALSE
end event

type ddlb_guarda from dropdownlistbox within w_cons_informe_llegadadestino
integer x = 3447
integer y = 552
integer width = 384
integer height = 400
integer taborder = 90
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean sorted = false
string item[] = {"Prolongada","Media","S/ Guarda"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF index = 1 THEN
	istr_mant.argumento[3] = '1'
ELSEIF index = 2 THEN
	istr_mant.argumento[3] = '2'
ELSE
	istr_mant.argumento[3] = '3'
END IF
cbx_todosgua.Checked = FALSE
end event

type ddlb_calidad from dropdownlistbox within w_cons_informe_llegadadestino
integer x = 2537
integer y = 548
integer width = 379
integer height = 368
integer taborder = 70
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean sorted = false
string item[] = {"Bueno","Regular","Malo"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF index = 1 THEN
	istr_mant.argumento[1] = '1'
ELSEIF index = 2 THEN
	istr_mant.argumento[1] = '2'
ELSE
	istr_mant.argumento[1] = '3'
END IF
cbx_todoscal.Checked = FALSE
end event

type st_9 from statictext within w_cons_informe_llegadadestino
integer x = 1367
integer y = 624
integer width = 160
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Hasta"
boolean focusrectangle = false
end type

type uo_muestraespecies from uo_seleccion_especie_mod within w_cons_informe_llegadadestino
integer x = 1143
integer y = 144
integer width = 1019
integer taborder = 50
end type

on uo_muestraespecies.destroy
call uo_seleccion_especie_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_muestravariedad.Todos(True)
			
		uo_muestravariedad.cbx_Todos.Enabled	=	False
				
	CASE ELSE
		uo_muestravariedad.Filtra(This.Codigo)
					
		uo_muestravariedad.cbx_Todos.Enabled	=	True
		
END CHOOSE
end event

type em_hasta from editmask within w_cons_informe_llegadadestino
integer x = 1536
integer y = 612
integer width = 320
integer height = 80
integer taborder = 80
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type em_desde from editmask within w_cons_informe_llegadadestino
integer x = 997
integer y = 612
integer width = 320
integer height = 80
integer taborder = 70
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_14 from statictext within w_cons_informe_llegadadestino
integer x = 736
integer y = 180
integer width = 206
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

type st_13 from statictext within w_cons_informe_llegadadestino
integer x = 3250
integer y = 560
integer width = 201
integer height = 64
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
string text = "Guarda"
boolean focusrectangle = false
end type

type st_12 from statictext within w_cons_informe_llegadadestino
integer x = 2258
integer y = 684
integer width = 261
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
string text = "Condición"
boolean focusrectangle = false
end type

type st_11 from statictext within w_cons_informe_llegadadestino
integer x = 2254
integer y = 568
integer width = 261
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Calidad"
boolean focusrectangle = false
end type

type uo_muestraprod from uo_seleccion_productor_mod within w_cons_informe_llegadadestino
integer x = 2939
integer y = 36
integer width = 1019
integer height = 120
integer taborder = 90
end type

on uo_muestraprod.destroy
call uo_seleccion_productor_mod::destroy
end on

type uo_muestrareci from uo_seleccion_recibidor_planilla_mod within w_cons_informe_llegadadestino
integer x = 1143
integer y = 356
integer width = 1019
integer taborder = 80
end type

on uo_muestrareci.destroy
call uo_seleccion_recibidor_planilla_mod::destroy
end on

type uo_muestramercado from uo_seleccion_mercado_mod within w_cons_informe_llegadadestino
integer x = 2939
integer y = 136
integer width = 1010
integer taborder = 40
end type

on uo_muestramercado.destroy
call uo_seleccion_mercado_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_muestrareci.Todos(True)
		uo_muestrareci.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		uo_muestrareci.Filtra(This.Codigo)
		uo_muestrareci.cbx_Todos.Enabled	=	True
END CHOOSE
end event

type uo_muestravariedad from uo_seleccion_variedad_mod within w_cons_informe_llegadadestino
integer x = 1143
integer y = 248
integer width = 1015
integer taborder = 40
end type

on uo_muestravariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type uo_muestrazona from uo_seleccion_zonas_mod within w_cons_informe_llegadadestino
integer x = 1143
integer y = 40
integer width = 1038
integer taborder = 30
end type

on uo_muestrazona.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_muestraprod.Todos(True)
		uo_muestraprod.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		uo_muestraprod.Filtra(This.Codigo)
		uo_muestraprod.cbx_Todos.Enabled	=	True
END CHOOSE
end event

type st_7 from statictext within w_cons_informe_llegadadestino
integer x = 736
integer y = 392
integer width = 247
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
string text = "Recibidor"
boolean focusrectangle = false
end type

type st_4 from statictext within w_cons_informe_llegadadestino
integer x = 2583
integer y = 64
integer width = 274
integer height = 80
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
string text = "Productor"
boolean focusrectangle = false
end type

type st_3 from statictext within w_cons_informe_llegadadestino
integer x = 2583
integer y = 164
integer width = 261
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Mercado"
boolean focusrectangle = false
end type

type st_5 from statictext within w_cons_informe_llegadadestino
integer x = 736
integer y = 80
integer width = 183
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
string text = "Zona"
boolean focusrectangle = false
end type

type uo_muestranave from uo_seleccion_naves_mod within w_cons_informe_llegadadestino
integer x = 2939
integer y = 236
integer width = 1015
integer taborder = 30
end type

on uo_muestranave.destroy
call uo_seleccion_naves_mod::destroy
end on

type st_6 from statictext within w_cons_informe_llegadadestino
integer x = 2583
integer y = 272
integer width = 242
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
string text = "Nave"
boolean focusrectangle = false
end type

type pb_imprimir from picturebutton within w_cons_informe_llegadadestino
string tag = "Selección de Parámetros"
integer x = 4713
integer y = 500
integer width = 233
integer height = 196
integer taborder = 70
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
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

type pb_buscar from picturebutton within w_cons_informe_llegadadestino
string tag = "Selección de Parámetros"
integer x = 4718
integer y = 296
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

event clicked;SetPointer(HourGlass!)

//Especies
   IF uo_muestraespecies.cbx_todos.checked THEN
	   Istr_mant.Argumento[4] 	= '-1'
   ELSE
		Istr_mant.Argumento[4]	= String(uo_muestraespecies.dw_Seleccion.Object.codigo[1])
		IF Istr_mant.Argumento[4] = "" THEN
			MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
			RETURN
		END IF
	END IF


//Variedad
   IF uo_muestravariedad.cbx_todos.checked THEN
	   Istr_mant.argumento[5] 	= '-1'
   ELSE
      Istr_mant.argumento[5]	= String(uo_muestravariedad.dw_Seleccion.Object.codigo[1])
	   IF Istr_mant.argumento[5] = "" THEN
	      MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF

//zona
   IF uo_muestrazona.cbx_todos.checked THEN
	   Istr_mant.argumento[6]	= '-1'
   ELSE
      Istr_mant.argumento[6]	= String(uo_muestrazona.dw_Seleccion.Object.codigo[1])
	   IF Istr_mant.argumento[6] = "" THEN
	      MessageBox("Atención","Debe Seleccionar una Zona Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF

//Mercado
   IF uo_muestramercado.cbx_todos.checked THEN
	   Istr_mant.argumento[7]	= '-1'
   ELSE
      Istr_mant.argumento[7]	= String(uo_muestramercado.dw_Seleccion.Object.codigo[1])
	   IF Istr_mant.argumento[7] = "" THEN
	      MessageBox("Atención","Debe Seleccionar un Mercado Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF

//Productor
   IF uo_muestraprod.cbx_todos.checked THEN
	   Istr_mant.argumento[8] 	= '-1'
   ELSE
      Istr_mant.argumento[8]	= String(uo_muestraprod.dw_Seleccion.Object.codigo[1])
	   IF Istr_mant.argumento[8] = "" THEN
	      MessageBox("Atención","Debe Seleccionar un Productor Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF

//Nave
   IF uo_muestranave.cbx_todos.checked THEN
	   Istr_mant.argumento[9] 	= '-1'
   ELSE
      Istr_mant.argumento[9]	= String(uo_muestranave.dw_Seleccion.Object.codigo[1])
	   IF Istr_mant.argumento[9] = "" THEN
	      MessageBox("Atención","Debe Seleccionar una nave Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF

//Recibidor
   IF uo_muestrareci.cbx_todos.checked THEN
	   Istr_mant.argumento[10] 	= '-1'
   ELSE
      Istr_mant.argumento[10]	= String(uo_muestrareci.dw_Seleccion.Object.codigo[1])
	   IF Istr_mant.argumento[10] = "" THEN
	      MessageBox("Atención","Debe Seleccionar un recibidor Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF

//Calidad
	IF istr_mant.argumento[1] = "" THEN
		MessageBox("Atención","Debe Seleccionar calificación de calidad Previamente",Exclamation!)
		RETURN
	END IF

//Condición
	IF istr_mant.argumento[2] = "" THEN
		MessageBox("Atención","Debe Seleccionar calificación de condición Previamente",Exclamation!)
		RETURN
	END IF

//Guarda
	IF istr_mant.argumento[3] = "" THEN
		MessageBox("Atención","Debe Seleccionar guarda caja Previamente",Exclamation!)
		RETURN
	END IF

//Fecha Recepción
IF cbx_todosfecha.Checked THEN
	  Istr_mant.argumento[11] = '01/01/2000'
	  Istr_mant.argumento[12] =  String(Today())
	  em_desde.text = Istr_mant.argumento[11]
	  em_hasta.Text = Istr_mant.argumento[12]
  ELSE
     Istr_mant.argumento[11] = em_desde.Text
	  Istr_mant.argumento[12] = em_hasta.Text
  END IF

	dw_1.Retrieve(Integer(Istr_mant.argumento[6]),Integer(Istr_mant.argumento[4]),&
					  Integer(Istr_mant.argumento[5]),Long(Istr_mant.argumento[8]),&
					  Integer(Istr_mant.argumento[7]),Integer(Istr_mant.argumento[9]),&
					  Long(Istr_mant.argumento[10]),Integer(Istr_mant.argumento[1]),&
					  Integer(Istr_mant.argumento[2]),Integer(Istr_mant.argumento[3]),&
					  Date(Istr_mant.argumento[11]),Date(Istr_mant.argumento[12]))
	
dw_1.SetRowFocusIndicator(Hand!)
dw_1.SetFocus()
		
IF dw_1.RowCount() = 0 THEN
	MessageBox('ADVERTENCIA','No Existe información')
	RETURN
ELSE
	pb_imprimir.Enabled	=	True
END IF
end event

type st_2 from statictext within w_cons_informe_llegadadestino
integer x = 736
integer y = 292
integer width = 270
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
string text = "Variedad"
boolean focusrectangle = false
end type

type dw_1 from uo_dw within w_cons_informe_llegadadestino
integer x = 37
integer y = 824
integer width = 4626
integer height = 1340
integer taborder = 0
boolean titlebar = true
string title = "Detalle Llegadas a Destino Por Productor"
string dataobject = "dw_cons_informe_llegadadestino"
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
	m_consulta.m_m_edicion.m_listadopornave.Visible	 					      =	False
	m_consulta.m_m_edicion.m_detalleplanilla.Visible					      =	True
	
	l_Menu.m_m_edicion.PopMenu(This.PointerX(),This.PointerY()+1100)
	
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

type pb_salir from picturebutton within w_cons_informe_llegadadestino
integer x = 4713
integer y = 1308
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

type pb_nuevo from picturebutton within w_cons_informe_llegadadestino
string tag = "Selección de Parámetros"
integer x = 4713
integer y = 1104
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

type gb_4 from groupbox within w_cons_informe_llegadadestino
boolean visible = false
integer x = 4704
integer y = 1012
integer width = 274
integer height = 504
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
end type

type gb_5 from groupbox within w_cons_informe_llegadadestino
boolean visible = false
integer x = 4704
integer y = 220
integer width = 274
integer height = 504
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
end type

type gb_2 from groupbox within w_cons_informe_llegadadestino
integer x = 2231
integer y = 500
integer width = 1915
integer height = 268
integer taborder = 70
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
end type

type gb_1 from groupbox within w_cons_informe_llegadadestino
integer x = 690
integer y = 500
integer width = 1531
integer height = 268
integer taborder = 80
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Fecha Arribo"
end type

type st_1 from statictext within w_cons_informe_llegadadestino
integer x = 658
integer y = 16
integer width = 3538
integer height = 792
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

