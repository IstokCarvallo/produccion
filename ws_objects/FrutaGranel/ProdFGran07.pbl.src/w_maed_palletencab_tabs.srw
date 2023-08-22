$PBExportHeader$w_maed_palletencab_tabs.srw
forward
global type w_maed_palletencab_tabs from w_mant_directo
end type
type em_fecha from editmask within w_maed_palletencab_tabs
end type
type st_4 from statictext within w_maed_palletencab_tabs
end type
type uo_selplanta from uo_seleccion_plantas within w_maed_palletencab_tabs
end type
type st_3 from statictext within w_maed_palletencab_tabs
end type
type st_2 from statictext within w_maed_palletencab_tabs
end type
type uo_selcliente from uo_seleccion_clientesprod within w_maed_palletencab_tabs
end type
type tab_1 from tab within w_maed_palletencab_tabs
end type
type tab_1 from tab within w_maed_palletencab_tabs
end type
end forward

global type w_maed_palletencab_tabs from w_mant_directo
integer width = 4110
integer height = 2360
string title = "PUCHOS EN EXISTENCIA"
em_fecha em_fecha
st_4 st_4
uo_selplanta uo_selplanta
st_3 st_3
st_2 st_2
uo_selcliente uo_selcliente
tab_1 tab_1
end type
global w_maed_palletencab_tabs w_maed_palletencab_tabs

type variables
uo_palletencab		iuo_tab[]

Long					il_indice[], il_tabs
Boolean				ib_buscar
end variables

forward prototypes
public function boolean creatabs ()
public subroutine cambiaicono ()
public function boolean validagrabacion ()
public subroutine nuevotab ()
public subroutine cambianombre ()
public subroutine escondetab ()
end prototypes

public function boolean creatabs ();Boolean 	lb_retorno = True
Integer	li_filas, li_fila

dw_1.SetSort("paen_numero asc")
dw_1.Sort()
tab_1.Visible	=	False

FOR li_filas =	1 TO dw_1.RowCount()
	il_tabs ++
	iuo_tab[il_tabs]					=	Create uo_palletencab
	tab_1.OpenTab(iuo_tab[il_tabs], 0)
	Tab_1.Control[il_tabs].Text	=	"Pallet: " + String(dw_1.Object.paen_numero[li_filas], '00000000')
	Tab_1.SelectTab(il_tabs)
	iuo_tab[il_tabs].CargaPallet(This, dw_1.Object.plde_codigo[li_filas], &
												  dw_1.Object.clie_codigo[li_filas], &
												  dw_1.Object.paen_numero[li_filas])
												  
	Tab_1.Control[il_tabs].Text			=	"Pallet: " + String(dw_1.Object.paen_numero[li_filas], '00000000')
	Tab_1.Control[il_tabs].PictureName	=	"Custom038!"
	Tab_1.Control[il_tabs].TabBackColor	=	RGB(195, 195, 195)
NEXT
tab_1.Visible	=	True
Return lb_retorno
end function

public subroutine cambiaicono ();Boolean 	lb_retorno = True
Integer	li_filas, li_fila

FOR li_filas =	1 TO UpperBound(iuo_tab)
	
	IF iuo_tab[li_filas].ii_estadoventana = 0 THEN
		Tab_1.Control[li_filas].PictureName	=	"Custom038!"
		
	ELSE
		Tab_1.Control[li_filas].PictureName	=	"Question!"
		
	END IF
	
NEXT
end subroutine

public function boolean validagrabacion ();Integer	li_filas

FOR li_filas = 1 TO UpperBound(iuo_tab)
	IF Tab_1.Control[li_filas].PictureName = "Question!" THEN
		Tab_1.SelectTab(li_filas)
		CHOOSE CASE MessageBox("Grabar registro(s)","¿Desea Grabar la información?", Question!, YesNoCancel!)
			CASE 1
				Message.DoubleParm = 0
				iuo_tab[Tab_1.SelectedTab].triggerevent("ue_guardar")
				IF message.doubleparm = -1 THEN Return FALSE
				RETURN ValidaGrabacion()
				
			CASE 3
				Return FALSE
				
		END CHOOSE
	END IF
NEXT

Return True
end function

public subroutine nuevotab ();tab_1.Visible	=	False

il_tabs ++
iuo_tab[il_tabs]							=	Create uo_palletencab
tab_1.OpenTab(iuo_tab[il_tabs], 0)

Tab_1.Control[il_tabs].Text			=	"Pallet: "
iuo_tab[il_tabs].iw_referencia		=	This
Tab_1.Control[il_tabs].TabBackColor	=	RGB(195, 195, 195)

Tab_1.SelectTab(il_tabs)

iuo_tab[il_tabs].TriggerEvent("ue_nuevo")

Tab_1.Control[il_tabs].PictureName	=	"Question!"
tab_1.Visible								=	True
end subroutine

public subroutine cambianombre ();
Tab_1.Control[Tab_1.SelectedTab].Text	=	"Pallet: " + String(iuo_tab[Tab_1.SelectedTab].dw_2.Object.paen_numero[1], '00000000')
end subroutine

public subroutine escondetab ();MessageBox("Información", "El pallet seleccionado alcanzo el total de cajas, se ocultará de esta ventana.")

Tab_1.Control[Tab_1.SelectedTab].Visible	=	False

IF Tab_1.SelectedTab - 1 >= LowerBound(Tab_1.Control) THEN
	Tab_1.SelectTab(Tab_1.SelectedTab - 1)
	
ELSEIF Tab_1.SelectedTab + 1 <= UpperBound(Tab_1.Control) THEN
	Tab_1.SelectTab(Tab_1.SelectedTab + 1)
	
END IF
end subroutine

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_Selplanta.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelPlanta.Seleccion(False,False)
	uo_SelCliente.Seleccion(False,False)
	
	uo_SelPlanta.dw_Seleccion.Object.codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
	uo_SelCliente.dw_Seleccion.Object.Codigo[1]		=	gi_CodExport
	
	uo_SelPlanta.Codigo										=	gstr_ParamPlanta.CodigoPlanta
	uo_SelCliente.Codigo										=	gi_CodExport
	
	em_fecha.Text												=	String(Today())
	
	ib_buscar													=	False
END IF
end event

on w_maed_palletencab_tabs.create
int iCurrent
call super::create
this.em_fecha=create em_fecha
this.st_4=create st_4
this.uo_selplanta=create uo_selplanta
this.st_3=create st_3
this.st_2=create st_2
this.uo_selcliente=create uo_selcliente
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_fecha
this.Control[iCurrent+2]=this.st_4
this.Control[iCurrent+3]=this.uo_selplanta
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.uo_selcliente
this.Control[iCurrent+7]=this.tab_1
end on

on w_maed_palletencab_tabs.destroy
call super::destroy
destroy(this.em_fecha)
destroy(this.st_4)
destroy(this.uo_selplanta)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.uo_selcliente)
destroy(this.tab_1)
end on

event ue_recuperadatos;Long		ll_fila
Integer	li_filas, respuesta

w_main.SetMicroHelp("Recuperando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

DO
	ll_fila		=	dw_1.Retrieve(uo_selcliente.Codigo, &
										  uo_selplanta.Codigo, &
										  Date(em_fecha.Text))
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		pb_eliminar.Enabled	=	True
		pb_insertar.Enabled	=	True
		CreaTabs()
		ib_buscar				=	True
	ELSE
		pb_insertar.Enabled	=	True
		
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event resize;Integer		li_posi_y, li_objeto

dw_1.Resize(This.WorkSpaceWidth() - 510,This.WorkSpaceHeight() - dw_1.y - 75)

dw_1.x					= 78
st_encabe.width		= dw_1.width

tab_1.x					= dw_1.x
tab_1.y					= dw_1.y
tab_1.width				= dw_1.width
tab_1.height			= dw_1.height

pb_lectura.x			= This.WorkSpaceWidth() - 292
pb_lectura.y			= 30
pb_lectura.width		= 156
pb_lectura.height		= 133

pb_nuevo.x				= This.WorkSpaceWidth() - 292
pb_nuevo.width			= 156
pb_nuevo.height		= 133

pb_insertar.x			= This.WorkSpaceWidth() - 292
pb_insertar.width		= 156
pb_insertar.height	= 133

pb_eliminar.x			= This.WorkSpaceWidth() - 292
pb_eliminar.width		= 156
pb_eliminar.height	= 133

pb_grabar.x				= This.WorkSpaceWidth() - 292
pb_grabar.width		= 156
pb_grabar.height		= 133

pb_imprimir.x			= This.WorkSpaceWidth() - 292
pb_imprimir.width		= 156
pb_imprimir.height	= 133

li_posi_y	= 1300

IF pb_nuevo.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_nuevo.y	= li_posi_y
END IF

IF pb_insertar.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_insertar.y	= li_posi_y
END IF

IF pb_eliminar.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_eliminar.y	= li_posi_y
END IF

IF pb_grabar.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_grabar.y		= li_posi_y
END IF

IF pb_imprimir.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_imprimir.y	= li_posi_y
END IF

pb_salir.x				= This.WorkSpaceWidth() - 292
pb_salir.y				= 1300
pb_salir.width			= 156
pb_salir.height		= 133
end event

event closequery;call super::closequery;Integer	li_fila

IF ib_buscar THEN
	IF NOT ValidaGrabacion() THEN
		Message.ReturnValue	=	1
	ELSE
		FOR li_fila = UpperBound(iuo_tab) TO 1
			tab_1.CloseTab(iuo_tab[li_fila])
			Destroy iuo_tab[li_fila];
		NEXT
		GarbageCollect()
	END IF
END IF
end event

event ue_nuevo;call super::ue_nuevo;NuevoTab()
end event

type st_encabe from w_mant_directo`st_encabe within w_maed_palletencab_tabs
integer y = 24
integer width = 3474
integer height = 192
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_maed_palletencab_tabs
boolean visible = false
integer x = 4384
integer y = 432
end type

type pb_lectura from w_mant_directo`pb_lectura within w_maed_palletencab_tabs
integer x = 3680
integer y = 96
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_maed_palletencab_tabs
string tag = "Eliminar Pallet"
boolean visible = false
integer x = 4005
integer y = 792
string powertiptext = "Eliminar Pallet"
end type

type pb_insertar from w_mant_directo`pb_insertar within w_maed_palletencab_tabs
string tag = "Ingresar Nuevo Pallet"
integer x = 3675
integer y = 628
string powertiptext = "Ingresar Nuevo Pallet"
end type

type pb_salir from w_mant_directo`pb_salir within w_maed_palletencab_tabs
integer x = 3675
integer y = 1536
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_maed_palletencab_tabs
boolean visible = false
integer x = 4384
integer y = 432
end type

type pb_grabar from w_mant_directo`pb_grabar within w_maed_palletencab_tabs
boolean visible = false
integer x = 4384
integer y = 432
end type

type dw_1 from w_mant_directo`dw_1 within w_maed_palletencab_tabs
boolean visible = false
integer width = 3474
integer height = 1520
string dataobject = "dw_lista_pallets_dia"
end type

type em_fecha from editmask within w_maed_palletencab_tabs
boolean visible = false
integer x = 4224
integer y = 352
integer width = 407
integer height = 88
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
boolean dropdownright = true
long calendartrailingtextcolor = 8421504
long calendarbackcolor = 12632256
end type

type st_4 from statictext within w_maed_palletencab_tabs
boolean visible = false
integer x = 4224
integer y = 364
integer width = 247
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Fecha"
boolean focusrectangle = false
end type

type uo_selplanta from uo_seleccion_plantas within w_maed_palletencab_tabs
integer x = 873
integer y = 72
integer height = 84
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_3 from statictext within w_maed_palletencab_tabs
integer x = 617
integer y = 80
integer width = 247
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_maed_palletencab_tabs
integer x = 1952
integer y = 80
integer width = 247
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_maed_palletencab_tabs
integer x = 2231
integer y = 72
integer height = 84
integer taborder = 50
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type tab_1 from tab within w_maed_palletencab_tabs
integer x = 78
integer y = 224
integer width = 3474
integer height = 2016
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean fixedwidth = true
boolean multiline = true
boolean raggedright = true
boolean focusonbuttondown = true
boolean powertips = true
boolean boldselectedtext = true
boolean pictureonright = true
integer selectedtab = 1
end type

event selectionchanged;CambiaIcono()
end event

