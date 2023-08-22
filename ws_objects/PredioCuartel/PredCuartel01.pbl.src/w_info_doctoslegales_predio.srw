$PBExportHeader$w_info_doctoslegales_predio.srw
$PBExportComments$Ventana de Informe de Resumen de palnillas de evaluación de descarte de fruta de packing
forward
global type w_info_doctoslegales_predio from w_para_informes
end type
type st_44 from statictext within w_info_doctoslegales_predio
end type
type st_5 from statictext within w_info_doctoslegales_predio
end type
type st_1 from statictext within w_info_doctoslegales_predio
end type
type st_2 from statictext within w_info_doctoslegales_predio
end type
type uo_muestrapredios from uo_seleccion_predioproduc_mod within w_info_doctoslegales_predio
end type
type uo_muestraproductor from uo_seleccion_productor_mod within w_info_doctoslegales_predio
end type
type st_3 from statictext within w_info_doctoslegales_predio
end type
end forward

global type w_info_doctoslegales_predio from w_para_informes
integer x = 14
integer y = 32
integer width = 1970
integer height = 724
string title = "INFORME - CONSERVADOR BIENES RAICES"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
//string icon = "\Desarrollo\Produccion\FrutaProcesada\Producc.ico"
st_44 st_44
st_5 st_5
st_1 st_1
st_2 st_2
uo_muestrapredios uo_muestrapredios
uo_muestraproductor uo_muestraproductor
st_3 st_3
end type
global w_info_doctoslegales_predio w_info_doctoslegales_predio

type variables
str_busqueda      istr_busq
str_mant         istr_mant
Integer	     ii_tipo
String	     is_report, is_nula


end variables

on w_info_doctoslegales_predio.create
int iCurrent
call super::create
this.st_44=create st_44
this.st_5=create st_5
this.st_1=create st_1
this.st_2=create st_2
this.uo_muestrapredios=create uo_muestrapredios
this.uo_muestraproductor=create uo_muestraproductor
this.st_3=create st_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_44
this.Control[iCurrent+2]=this.st_5
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.uo_muestrapredios
this.Control[iCurrent+6]=this.uo_muestraproductor
this.Control[iCurrent+7]=this.st_3
end on

on w_info_doctoslegales_predio.destroy
call super::destroy
destroy(this.st_44)
destroy(this.st_5)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.uo_muestrapredios)
destroy(this.uo_muestraproductor)
destroy(this.st_3)
end on

event open;x = 0
y = 0
This.Icon	=	Gstr_apl.Icono

Boolean	lb_Cerrar

IF IsNull(uo_muestrapredios.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestraproductor.Codigo) THEN lb_Cerrar	=	True


IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_muestrapredios.Seleccion(True, True)
	uo_muestraproductor.Seleccion(True, True)
	
	uo_muestraproductor.cbx_consolida.checked = False
	uo_muestraproductor.cbx_todos.checked     = True
	uo_muestraproductor.dw_seleccion.enabled  = True
	
	uo_muestrapredios.cbx_consolida.checked = False
	uo_muestrapredios.cbx_todos.checked     = True
	uo_muestrapredios.dw_seleccion.enabled  = True
	uo_muestrapredios.filtra(0)
END IF
end event

type st_computador from w_para_informes`st_computador within w_info_doctoslegales_predio
end type

type st_usuario from w_para_informes`st_usuario within w_info_doctoslegales_predio
end type

type st_temporada from w_para_informes`st_temporada within w_info_doctoslegales_predio
end type

type p_logo from w_para_informes`p_logo within w_info_doctoslegales_predio
end type

type st_titulo from w_para_informes`st_titulo within w_info_doctoslegales_predio
boolean visible = false
integer x = 37
integer y = 64
integer width = 1833
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_doctoslegales_predio
string tag = "Imprimir Reporte"
integer x = 1701
integer y = 96
integer taborder = 30
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	li_fila, li_predio
Long ll_productor

SetPointer(HourGlass!)

IF uo_muestraproductor.cbx_todos.checked THEN
	ll_productor = -1
ELSE
	ll_productor	= uo_muestraproductor.dw_Seleccion.Object.codigo[1]
	IF IsNull(ll_productor)THEN
		MessageBox("Atención","Debe Seleccionar un Productor Previamente",Exclamation!)
		RETURN
	END IF
END IF


IF uo_muestrapredios.cbx_todos.checked THEN
	li_predio = -1
ELSE
	li_predio	= uo_muestrapredios.dw_Seleccion.Object.codigo[1]
	IF IsNull(li_predio)THEN
		MessageBox("Atención","Debe Seleccionar un Predio Previamente",Exclamation!)
		RETURN
	END IF
END IF

istr_info.titulo	= 'INFORME - CONSERVADOR BIENES RAICES'

OpenWithParm(vinf,istr_info)

  vinf.dw_1.DataObject = "dw_info_doctoslegalespredios"

vinf.dw_1.SetTransObject(sqlca)
li_fila = vinf.dw_1.Retrieve(ll_productor,li_predio)

IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("t_numero.text = '" + '5' + "'")

	
	vinf.dw_1.Object.DataWindow.Zoom = 98
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_doctoslegales_predio
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1701
integer y = 376
integer taborder = 40
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_44 from statictext within w_info_doctoslegales_predio
integer x = 37
integer y = 136
integer width = 1531
integer height = 456
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_doctoslegales_predio
integer x = 37
integer y = 32
integer width = 1531
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Conservador Bienes Raices"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_doctoslegales_predio
integer x = 101
integer y = 400
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Predio"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_doctoslegales_predio
integer x = 1289
integer y = 184
integer width = 210
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todos"
boolean focusrectangle = false
end type

type uo_muestrapredios from uo_seleccion_predioproduc_mod within w_info_doctoslegales_predio
integer x = 421
integer y = 372
integer width = 1024
integer taborder = 20
boolean bringtotop = true
end type

on uo_muestrapredios.destroy
call uo_seleccion_predioproduc_mod::destroy
end on

type uo_muestraproductor from uo_seleccion_productor_mod within w_info_doctoslegales_predio
integer x = 425
integer y = 264
integer width = 1029
integer taborder = 10
boolean bringtotop = true
end type

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1
		uo_muestrapredios.Todos(True)
		uo_muestrapredios.cbx_Todos.Enabled	=	False
		uo_muestrapredios.cbx_todos.checked = True
	   uo_muestrapredios.dw_seleccion.enabled  = False
				
	CASE ELSE
		uo_muestrapredios.Filtra(This.Codigo)
		uo_muestrapredios.cbx_Todos.Enabled	=	True
		uo_muestrapredios.dw_seleccion.enabled  = True
		
END CHOOSE
end event

on uo_muestraproductor.destroy
call uo_seleccion_productor_mod::destroy
end on

type st_3 from statictext within w_info_doctoslegales_predio
integer x = 101
integer y = 292
integer width = 297
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Productor"
boolean focusrectangle = false
end type

