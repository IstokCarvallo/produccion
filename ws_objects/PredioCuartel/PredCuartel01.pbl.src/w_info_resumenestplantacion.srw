$PBExportHeader$w_info_resumenestplantacion.srw
$PBExportComments$Ventana de Informe de Resumen de palnillas de evaluación de descarte de fruta de packing
forward
global type w_info_resumenestplantacion from w_para_informes
end type
type st_zona from statictext within w_info_resumenestplantacion
end type
type st_33 from statictext within w_info_resumenestplantacion
end type
type st_14 from statictext within w_info_resumenestplantacion
end type
type st_2 from statictext within w_info_resumenestplantacion
end type
type uo_muestrazona from uo_seleccion_zonas_mod within w_info_resumenestplantacion
end type
type uo_muestraespecies from uo_seleccion_especie_mod within w_info_resumenestplantacion
end type
type st_1 from statictext within w_info_resumenestplantacion
end type
type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_resumenestplantacion
end type
type st_3 from statictext within w_info_resumenestplantacion
end type
type uo_muestraproductor from uo_seleccion_productor_mod within w_info_resumenestplantacion
end type
type st_6 from statictext within w_info_resumenestplantacion
end type
type uo_muestraagronomo from uo_seleccion_agronomo_mod within w_info_resumenestplantacion
end type
type uo_muestratipoprod from uo_seleccion_tipoproductor_mod within w_info_resumenestplantacion
end type
type st_4 from statictext within w_info_resumenestplantacion
end type
type st_5 from statictext within w_info_resumenestplantacion
end type
end forward

global type w_info_resumenestplantacion from w_para_informes
string tag = "Consulta Resumen Estadisticas de Plantación - Predios/Cuarteles"
integer x = 14
integer y = 32
integer width = 2656
integer height = 1420
string title = "RESUMEN - ESTADISTICAS DE PLANTACION PREDIOS CUARTELES"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
st_zona st_zona
st_33 st_33
st_14 st_14
st_2 st_2
uo_muestrazona uo_muestrazona
uo_muestraespecies uo_muestraespecies
st_1 st_1
uo_muestravariedad uo_muestravariedad
st_3 st_3
uo_muestraproductor uo_muestraproductor
st_6 st_6
uo_muestraagronomo uo_muestraagronomo
uo_muestratipoprod uo_muestratipoprod
st_4 st_4
st_5 st_5
end type
global w_info_resumenestplantacion w_info_resumenestplantacion

type variables
str_busqueda      istr_busq
str_mant         istr_mant
Integer	     ii_tipo
String	     is_report, is_nula


end variables

on w_info_resumenestplantacion.create
int iCurrent
call super::create
this.st_zona=create st_zona
this.st_33=create st_33
this.st_14=create st_14
this.st_2=create st_2
this.uo_muestrazona=create uo_muestrazona
this.uo_muestraespecies=create uo_muestraespecies
this.st_1=create st_1
this.uo_muestravariedad=create uo_muestravariedad
this.st_3=create st_3
this.uo_muestraproductor=create uo_muestraproductor
this.st_6=create st_6
this.uo_muestraagronomo=create uo_muestraagronomo
this.uo_muestratipoprod=create uo_muestratipoprod
this.st_4=create st_4
this.st_5=create st_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_zona
this.Control[iCurrent+2]=this.st_33
this.Control[iCurrent+3]=this.st_14
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.uo_muestrazona
this.Control[iCurrent+6]=this.uo_muestraespecies
this.Control[iCurrent+7]=this.st_1
this.Control[iCurrent+8]=this.uo_muestravariedad
this.Control[iCurrent+9]=this.st_3
this.Control[iCurrent+10]=this.uo_muestraproductor
this.Control[iCurrent+11]=this.st_6
this.Control[iCurrent+12]=this.uo_muestraagronomo
this.Control[iCurrent+13]=this.uo_muestratipoprod
this.Control[iCurrent+14]=this.st_4
this.Control[iCurrent+15]=this.st_5
end on

on w_info_resumenestplantacion.destroy
call super::destroy
destroy(this.st_zona)
destroy(this.st_33)
destroy(this.st_14)
destroy(this.st_2)
destroy(this.uo_muestrazona)
destroy(this.uo_muestraespecies)
destroy(this.st_1)
destroy(this.uo_muestravariedad)
destroy(this.st_3)
destroy(this.uo_muestraproductor)
destroy(this.st_6)
destroy(this.uo_muestraagronomo)
destroy(this.uo_muestratipoprod)
destroy(this.st_4)
destroy(this.st_5)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_muestrazona.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestraproductor.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestratipoprod.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestraespecies.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestravariedad.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestraagronomo.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_muestrazona.Seleccion(True, True)
	uo_muestraproductor.Seleccion(True, True)
	uo_muestratipoprod.Seleccion(True, True)
	uo_muestraespecies.Seleccion(True, True)
	uo_muestravariedad.Seleccion(True, True)
	uo_muestraagronomo.Seleccion(True, True)
	
	uo_muestrazona.cbx_todos.checked     = True
	uo_muestrazona.cbx_consolida.checked = False
	uo_muestrazona.dw_seleccion.enabled  = True

	uo_muestraproductor.cbx_consolida.checked = False
	uo_muestraproductor.cbx_todos.checked     = True
	uo_muestraproductor.dw_seleccion.enabled  = True
	uo_muestraproductor.filtra(0)
	
	uo_muestratipoprod.cbx_todos.checked     = True
	uo_muestratipoprod.cbx_consolida.checked = False
	uo_muestratipoprod.dw_seleccion.enabled  = True
	
	uo_muestraespecies.cbx_todos.checked      = False
	uo_muestraespecies.cbx_todos.visible      = True
	uo_muestraespecies.cbx_consolida.visible  = False
	uo_muestraespecies.cbx_consolida.checked  = False
	uo_muestraespecies.dw_seleccion.enabled   = True
	uo_muestraespecies.dw_seleccion.Object.codigo[1] = 11
	
	uo_muestravariedad.cbx_todos.checked     = True
	uo_muestravariedad.cbx_consolida.checked = False
	uo_muestravariedad.dw_seleccion.enabled  = True
	uo_muestravariedad.filtra(11)
	
	uo_muestraagronomo.cbx_consolida.checked = False
	uo_muestraagronomo.cbx_todos.checked     = True
	uo_muestraagronomo.cbx_todos.Enabled     = True
	uo_muestraagronomo.dw_seleccion.enabled  = True
	uo_muestraagronomo.filtra(-1)
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_info_resumenestplantacion
end type

type st_computador from w_para_informes`st_computador within w_info_resumenestplantacion
end type

type st_usuario from w_para_informes`st_usuario within w_info_resumenestplantacion
end type

type st_temporada from w_para_informes`st_temporada within w_info_resumenestplantacion
end type

type p_logo from w_para_informes`p_logo within w_info_resumenestplantacion
end type

type st_titulo from w_para_informes`st_titulo within w_info_resumenestplantacion
integer x = 261
integer y = 264
integer width = 1833
integer height = 88
string text = "Resumen Estadistica de Plantación"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resumenestplantacion
string tag = "Imprimir Reporte"
integer x = 2226
integer y = 480
integer taborder = 190
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	li_fila, li_zona, li_especie, li_variedad, li_tipoprod  
Long     ll_Productor,ll_agronomo

SetPointer(HourGlass!)

//zona
IF uo_muestrazona.cbx_consolida.checked THEN
	li_zona   = -9
ELSE
  IF uo_muestrazona.cbx_todos.checked THEN
	   li_zona	= -1
   ELSE
      li_zona	= uo_muestrazona.dw_Seleccion.Object.codigo[1]
	   IF IsNull(li_zona)THEN
	      MessageBox("Atención","Debe Seleccionar una Zona Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF
END IF
	
	
//Productor
IF uo_muestraproductor.cbx_consolida.checked THEN
	ll_productor   = -9
ELSE
 IF uo_muestraproductor.cbx_todos.checked THEN
	   ll_productor = -1
   ELSE
      ll_productor = uo_muestraproductor.dw_Seleccion.Object.codigo[1]
	   IF IsNull(ll_productor)THEN
	      MessageBox("Atención","Debe Seleccionar un Productor Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF
END IF

//Tipo Productor
IF uo_muestratipoprod.cbx_consolida.checked THEN
	li_tipoprod   = -9
ELSE
  IF uo_muestratipoprod.cbx_todos.checked THEN
	   li_tipoprod = -1
   ELSE
      li_tipoprod = uo_muestratipoprod.dw_Seleccion.Object.codigo[1]
	   IF IsNull(li_tipoprod)THEN
	      MessageBox("Atención","Debe Seleccionar un Tipo de Productor Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF
END IF


//Especies
IF uo_muestraespecies.cbx_todos.checked THEN
	   li_especie = -1
   ELSE
		li_especie	= uo_muestraespecies.dw_Seleccion.Object.codigo[1]
		IF IsNull(li_especie)THEN
			MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
			RETURN
		END IF
END IF

//Variedad
  IF uo_muestravariedad.cbx_todos.checked THEN
	   li_variedad 	= -1
   ELSE
      li_variedad	= uo_muestravariedad.dw_Seleccion.Object.codigo[1]
	   IF IsNull(li_variedad)THEN
	      MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF

//Agronomo
IF uo_muestraagronomo.cbx_consolida.checked THEN
	ll_agronomo   = -9
ELSE
  IF uo_muestraagronomo.cbx_todos.checked THEN
	   ll_agronomo 	= -1
   ELSE
      ll_agronomo	= uo_muestraagronomo.dw_Seleccion.Object.codigo[1]
	   IF IsNull(ll_agronomo)THEN
	      MessageBox("Atención","Debe Seleccionar un Agrónomo Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF
END IF
	
istr_info.titulo	= 'RESUMEN - ESTADISTICA DE PLANTACION'
OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_resumenestaplantacion"
vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(li_zona,ll_productor,li_tipoprod,li_especie,li_variedad,ll_agronomo)

IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("t_numero.text = '" + '2' + "'")
	vinf.dw_1.Object.DataWindow.Zoom = 98
	
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_resumenestplantacion
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2226
integer y = 760
integer taborder = 200
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_zona from statictext within w_info_resumenestplantacion
integer x = 293
integer y = 552
integer width = 370
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
string text = "Zona"
boolean focusrectangle = false
end type

type st_33 from statictext within w_info_resumenestplantacion
integer x = 297
integer y = 672
integer width = 370
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
string text = "Productor"
boolean focusrectangle = false
end type

type st_14 from statictext within w_info_resumenestplantacion
integer x = 293
integer y = 928
integer width = 370
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
string text = "Especie"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_resumenestplantacion
integer x = 1609
integer y = 460
integer width = 197
integer height = 60
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
string text = "Todos"
boolean focusrectangle = false
end type

type uo_muestrazona from uo_seleccion_zonas_mod within w_info_resumenestplantacion
integer x = 750
integer y = 516
integer width = 1193
integer height = 108
integer taborder = 130
boolean bringtotop = true
end type

on uo_muestrazona.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_muestraagronomo.Todos(True)
		uo_muestraagronomo.cbx_Todos.Enabled	=	False
		uo_muestraagronomo.Filtra(This.Codigo)
		
	CASE ELSE
		uo_muestraagronomo.Filtra(This.Codigo)
		uo_muestraagronomo.cbx_Todos.Enabled	=	True
END CHOOSE
end event

type uo_muestraespecies from uo_seleccion_especie_mod within w_info_resumenestplantacion
event destroy ( )
integer x = 750
integer y = 892
integer width = 992
integer height = 108
integer taborder = 150
boolean bringtotop = true
end type

on uo_muestraespecies.destroy
call uo_seleccion_especie_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_muestravariedad.Todos(True)
		uo_muestravariedad.cbx_Todos.Enabled	=	TRUE
				
	CASE ELSE
		uo_muestravariedad.Filtra(This.Codigo)
		uo_muestravariedad.cbx_Todos.Enabled	=	True
		
END CHOOSE
end event

type st_1 from statictext within w_info_resumenestplantacion
integer x = 297
integer y = 800
integer width = 443
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tipo Productor"
boolean focusrectangle = false
end type

type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_resumenestplantacion
integer x = 745
integer y = 1020
integer width = 1001
integer height = 108
integer taborder = 160
boolean bringtotop = true
end type

on uo_muestravariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type st_3 from statictext within w_info_resumenestplantacion
integer x = 293
integer y = 1060
integer width = 370
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
string text = "Variedad"
boolean focusrectangle = false
end type

type uo_muestraproductor from uo_seleccion_productor_mod within w_info_resumenestplantacion
event destroy ( )
integer x = 750
integer y = 636
integer width = 1193
integer height = 108
integer taborder = 140
boolean bringtotop = true
end type

on uo_muestraproductor.destroy
call uo_seleccion_productor_mod::destroy
end on

type st_6 from statictext within w_info_resumenestplantacion
integer x = 293
integer y = 1184
integer width = 402
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
string text = "Agrónomo"
boolean focusrectangle = false
end type

type uo_muestraagronomo from uo_seleccion_agronomo_mod within w_info_resumenestplantacion
integer x = 745
integer y = 1152
integer width = 1193
integer height = 124
integer taborder = 170
boolean bringtotop = true
end type

on uo_muestraagronomo.destroy
call uo_seleccion_agronomo_mod::destroy
end on

type uo_muestratipoprod from uo_seleccion_tipoproductor_mod within w_info_resumenestplantacion
integer x = 750
integer y = 764
integer width = 1193
integer height = 120
integer taborder = 210
boolean bringtotop = true
end type

on uo_muestratipoprod.destroy
call uo_seleccion_tipoproductor_mod::destroy
end on

type st_4 from statictext within w_info_resumenestplantacion
integer x = 1833
integer y = 460
integer width = 197
integer height = 60
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
string text = "Cons."
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_resumenestplantacion
integer x = 247
integer y = 428
integer width = 1810
integer height = 884
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = StyleRaised!
boolean focusrectangle = false
end type

