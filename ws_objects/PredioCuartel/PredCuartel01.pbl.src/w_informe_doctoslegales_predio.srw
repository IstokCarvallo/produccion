$PBExportHeader$w_informe_doctoslegales_predio.srw
$PBExportComments$Ventana de Informe de Resumen de palnillas de evaluación de descarte de fruta de packing
forward
global type w_informe_doctoslegales_predio from w_para_informes
end type
type st_zona from statictext within w_informe_doctoslegales_predio
end type
type st_33 from statictext within w_informe_doctoslegales_predio
end type
type st_14 from statictext within w_informe_doctoslegales_predio
end type
type st_2 from statictext within w_informe_doctoslegales_predio
end type
type uo_muestrazona from uo_seleccion_zonas_mod within w_informe_doctoslegales_predio
end type
type uo_muestraespecies from uo_seleccion_especie_mod within w_informe_doctoslegales_predio
end type
type uo_muestravariedad from uo_seleccion_variedad_mod within w_informe_doctoslegales_predio
end type
type st_3 from statictext within w_informe_doctoslegales_predio
end type
type uo_muestraproductor from uo_seleccion_productor_mod within w_informe_doctoslegales_predio
end type
type st_5 from statictext within w_informe_doctoslegales_predio
end type
type st_44 from statictext within w_informe_doctoslegales_predio
end type
type st_1 from statictext within w_informe_doctoslegales_predio
end type
type st_4 from statictext within w_informe_doctoslegales_predio
end type
type uo_muestrapredios from uo_seleccion_predioproduc_mod within w_informe_doctoslegales_predio
end type
type uo_muestracuarteles from uo_seleccion_prodcuarteles_mod within w_informe_doctoslegales_predio
end type
end forward

global type w_informe_doctoslegales_predio from w_para_informes
string tag = "Consulta doctos conservador bienes raices - Predios/Cuarteles"
integer x = 14
integer y = 32
integer width = 2533
integer height = 1648
string title = "INFORME - DOCUMENTOS CONSERVADOR BIENES RAICES"
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
uo_muestravariedad uo_muestravariedad
st_3 st_3
uo_muestraproductor uo_muestraproductor
st_5 st_5
st_44 st_44
st_1 st_1
st_4 st_4
uo_muestrapredios uo_muestrapredios
uo_muestracuarteles uo_muestracuarteles
end type
global w_informe_doctoslegales_predio w_informe_doctoslegales_predio

type variables
str_busqueda      istr_busq
str_mant         istr_mant

Integer	     ii_tipo
String	     is_report, is_nula
Long          il_productor 


end variables

on w_informe_doctoslegales_predio.create
int iCurrent
call super::create
this.st_zona=create st_zona
this.st_33=create st_33
this.st_14=create st_14
this.st_2=create st_2
this.uo_muestrazona=create uo_muestrazona
this.uo_muestraespecies=create uo_muestraespecies
this.uo_muestravariedad=create uo_muestravariedad
this.st_3=create st_3
this.uo_muestraproductor=create uo_muestraproductor
this.st_5=create st_5
this.st_44=create st_44
this.st_1=create st_1
this.st_4=create st_4
this.uo_muestrapredios=create uo_muestrapredios
this.uo_muestracuarteles=create uo_muestracuarteles
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_zona
this.Control[iCurrent+2]=this.st_33
this.Control[iCurrent+3]=this.st_14
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.uo_muestrazona
this.Control[iCurrent+6]=this.uo_muestraespecies
this.Control[iCurrent+7]=this.uo_muestravariedad
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.uo_muestraproductor
this.Control[iCurrent+10]=this.st_5
this.Control[iCurrent+11]=this.st_44
this.Control[iCurrent+12]=this.st_1
this.Control[iCurrent+13]=this.st_4
this.Control[iCurrent+14]=this.uo_muestrapredios
this.Control[iCurrent+15]=this.uo_muestracuarteles
end on

on w_informe_doctoslegales_predio.destroy
call super::destroy
destroy(this.st_zona)
destroy(this.st_33)
destroy(this.st_14)
destroy(this.st_2)
destroy(this.uo_muestrazona)
destroy(this.uo_muestraespecies)
destroy(this.uo_muestravariedad)
destroy(this.st_3)
destroy(this.uo_muestraproductor)
destroy(this.st_5)
destroy(this.st_44)
destroy(this.st_1)
destroy(this.st_4)
destroy(this.uo_muestrapredios)
destroy(this.uo_muestracuarteles)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_muestrazona.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestraproductor.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestraespecies.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestravariedad.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestrapredios.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_muestracuarteles.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_muestrazona.Seleccion(True, True)
	uo_muestraproductor.Seleccion(True, True)
	uo_muestraespecies.Seleccion(True, True)
	uo_muestravariedad.Seleccion(True, True)
	uo_muestrapredios.Seleccion(True, True)
	uo_muestracuarteles.Seleccion(True, True)
	
	uo_muestraproductor.filtra(-1)
	
	uo_muestraespecies.dw_seleccion.Object.codigo[1] = 11
	uo_muestraespecies.Codigo = 11
	
	uo_muestravariedad.filtra(11)
	
	uo_muestrapredios.filtra(-1)
	uo_muestracuarteles.filtra(-1,-1)
	
	
		uo_muestracuarteles.Todos(True)
		uo_muestracuarteles.cbx_Todos.Enabled	=	False
		uo_muestrapredios.cbx_Todos.Enabled	=	False
		uo_muestrapredios.dw_seleccion.enabled  = False
		uo_muestrapredios.cbx_todos.checked = True
		uo_muestracuarteles.cbx_todos.checked = True
	   	uo_muestracuarteles.dw_seleccion.enabled  = False
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_informe_doctoslegales_predio
end type

type st_computador from w_para_informes`st_computador within w_informe_doctoslegales_predio
end type

type st_usuario from w_para_informes`st_usuario within w_informe_doctoslegales_predio
end type

type st_temporada from w_para_informes`st_temporada within w_informe_doctoslegales_predio
end type

type p_logo from w_para_informes`p_logo within w_informe_doctoslegales_predio
end type

type st_titulo from w_para_informes`st_titulo within w_informe_doctoslegales_predio
boolean visible = false
integer y = 344
integer width = 1833
end type

type pb_acepta from w_para_informes`pb_acepta within w_informe_doctoslegales_predio
string tag = "Imprimir Reporte"
integer x = 2217
integer y = 484
integer taborder = 190
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	li_fila, li_zona, li_especie, li_variedad, li_predio, li_cuartel
Long     ll_Productor

SetPointer(HourGlass!)

//zona
  IF uo_muestrazona.cbx_todos.checked THEN
	   li_zona	= -1
   ELSE
      li_zona	= uo_muestrazona.dw_Seleccion.Object.codigo[1]
	   IF IsNull(li_zona)THEN
	      MessageBox("Atención","Debe Seleccionar una Zona Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF


//Productor
 IF uo_muestraproductor.cbx_todos.checked THEN
	   ll_productor = -1
   ELSE
      ll_productor = uo_muestraproductor.dw_Seleccion.Object.codigo[1]
	   IF IsNull(ll_productor)THEN
	      MessageBox("Atención","Debe Seleccionar un Productor Previamente",Exclamation!)
	      RETURN
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
	
//Predio
  IF uo_muestrapredios.cbx_todos.checked THEN
	   li_predio 	= -1
   ELSE
      li_predio	= uo_muestrapredios.dw_Seleccion.Object.codigo[1]
	   IF IsNull(li_predio)THEN
	      MessageBox("Atención","Debe Seleccionar un Predio Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF
	
	//cuartel
  IF uo_muestracuarteles.cbx_todos.checked THEN
	   li_cuartel 	= -1
   ELSE
      li_cuartel	= uo_muestracuarteles.dw_Seleccion.Object.codigo[1]
	   IF IsNull(li_cuartel)THEN
	      MessageBox("Atención","Debe Seleccionar un Cuartel Previamente",Exclamation!)
	      RETURN
	   END IF
   END IF

istr_info.titulo	= 'INFORME - DOCUMENTOS CONSERVADOR BIENES RAICES'
OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_doctoslegalespredios"
vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(li_zona,ll_productor,li_especie,li_variedad, li_predio,li_cuartel)

IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("t_numero.text = '" + '5' + "'")
	vinf.dw_1.Object.DataWindow.Zoom = 82
	
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_informe_doctoslegales_predio
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2217
integer y = 764
integer taborder = 200
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_zona from statictext within w_informe_doctoslegales_predio
integer x = 379
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

type st_33 from statictext within w_informe_doctoslegales_predio
integer x = 379
integer y = 676
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

type st_14 from statictext within w_informe_doctoslegales_predio
integer x = 379
integer y = 796
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

type st_2 from statictext within w_informe_doctoslegales_predio
integer x = 1696
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

type uo_muestrazona from uo_seleccion_zonas_mod within w_informe_doctoslegales_predio
integer x = 832
integer y = 516
integer width = 1056
integer height = 112
integer taborder = 130
boolean bringtotop = true
end type

on uo_muestrazona.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;//IF IsNull(This.Codigo) THEN RETURN
//
//CHOOSE CASE This.Codigo
//	CASE -1, -9
//		uo_muestraagronomo.Todos(True)
//		uo_muestraagronomo.cbx_Todos.Enabled	=	False
//		
//	CASE ELSE
//		
//		uo_muestraagronomo.Filtra(This.Codigo)
//		uo_muestraagronomo.cbx_Todos.Enabled	=	True
//END CHOOSE
end event

type uo_muestraespecies from uo_seleccion_especie_mod within w_informe_doctoslegales_predio
event destroy ( )
integer x = 832
integer y = 760
integer width = 1019
integer height = 112
integer taborder = 150
boolean bringtotop = true
end type

on uo_muestraespecies.destroy
call uo_seleccion_especie_mod::destroy
end on

event ue_cambio;call super::ue_cambio;
IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_muestravariedad.Todos(True)
		uo_muestravariedad.cbx_Todos.Enabled	=	False
				
	CASE ELSE
		uo_muestravariedad.Filtra(This.Codigo)
		uo_muestravariedad.cbx_Todos.Enabled	=	True
		
END CHOOSE
end event

type uo_muestravariedad from uo_seleccion_variedad_mod within w_informe_doctoslegales_predio
integer x = 832
integer y = 888
integer width = 1024
integer height = 112
integer taborder = 160
boolean bringtotop = true
end type

on uo_muestravariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type st_3 from statictext within w_informe_doctoslegales_predio
integer x = 379
integer y = 928
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

type uo_muestraproductor from uo_seleccion_productor_mod within w_informe_doctoslegales_predio
event destroy ( )
integer x = 832
integer y = 632
integer width = 1033
integer height = 112
integer taborder = 140
boolean bringtotop = true
end type

on uo_muestraproductor.destroy
call uo_seleccion_productor_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN
	RETURN
ELSE
	il_productor = This.Codigo
END IF

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

type st_5 from statictext within w_informe_doctoslegales_predio
integer x = 251
integer y = 284
integer width = 1833
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Documentos Conservador Bienes Raices"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_44 from statictext within w_informe_doctoslegales_predio
integer x = 251
integer y = 440
integer width = 1833
integer height = 928
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_informe_doctoslegales_predio
integer x = 379
integer y = 1060
integer width = 288
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
string text = "Predio"
boolean focusrectangle = false
end type

type st_4 from statictext within w_informe_doctoslegales_predio
integer x = 379
integer y = 1184
integer width = 320
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
string text = "Cuartel"
boolean focusrectangle = false
end type

type uo_muestrapredios from uo_seleccion_predioproduc_mod within w_informe_doctoslegales_predio
integer x = 832
integer y = 1012
integer width = 1010
integer taborder = 170
boolean bringtotop = true
end type

on uo_muestrapredios.destroy
call uo_seleccion_predioproduc_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1
		uo_muestracuarteles.Todos(True)
		uo_muestracuarteles.cbx_Todos.Enabled	=	False
		uo_muestracuarteles.cbx_todos.checked = True
	   uo_muestracuarteles.dw_seleccion.enabled  = False
				
	CASE ELSE
		uo_muestracuarteles.Filtra(il_productor,This.Codigo)
		uo_muestracuarteles.cbx_Todos.Enabled	=	True
		uo_muestracuarteles.dw_seleccion.enabled  = True
		
END CHOOSE
end event

type uo_muestracuarteles from uo_seleccion_prodcuarteles_mod within w_informe_doctoslegales_predio
integer x = 832
integer y = 1144
integer width = 1024
integer taborder = 180
boolean bringtotop = true
end type

on uo_muestracuarteles.destroy
call uo_seleccion_prodcuarteles_mod::destroy
end on

