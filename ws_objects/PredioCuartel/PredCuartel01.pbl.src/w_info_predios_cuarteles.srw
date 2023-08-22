$PBExportHeader$w_info_predios_cuarteles.srw
$PBExportComments$Ventana de Informe de Resumen de palnillas de evaluación de descarte de fruta de packing
forward
global type w_info_predios_cuarteles from w_para_informes
end type
type st_zona from statictext within w_info_predios_cuarteles
end type
type st_33 from statictext within w_info_predios_cuarteles
end type
type st_14 from statictext within w_info_predios_cuarteles
end type
type st_2 from statictext within w_info_predios_cuarteles
end type
type uo_muestrazona from uo_seleccion_zonas_mod within w_info_predios_cuarteles
end type
type uo_muestraespecies from uo_seleccion_especie_mod within w_info_predios_cuarteles
end type
type st_1 from statictext within w_info_predios_cuarteles
end type
type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_predios_cuarteles
end type
type st_3 from statictext within w_info_predios_cuarteles
end type
type st_44 from statictext within w_info_predios_cuarteles
end type
type uo_muestraproductor from uo_seleccion_productor_mod within w_info_predios_cuarteles
end type
type st_5 from statictext within w_info_predios_cuarteles
end type
type st_6 from statictext within w_info_predios_cuarteles
end type
type uo_muestraagronomo from uo_seleccion_agronomo_mod within w_info_predios_cuarteles
end type
type uo_muestratipoprod from uo_seleccion_tipoproductor_mod within w_info_predios_cuarteles
end type
end forward

global type w_info_predios_cuarteles from w_para_informes
integer x = 14
integer y = 32
integer width = 2638
integer height = 1604
string title = "INFORME - PREDIOS CUARTELES"
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
st_44 st_44
uo_muestraproductor uo_muestraproductor
st_5 st_5
st_6 st_6
uo_muestraagronomo uo_muestraagronomo
uo_muestratipoprod uo_muestratipoprod
end type
global w_info_predios_cuarteles w_info_predios_cuarteles

type variables
str_busqueda      istr_busq
str_mant         istr_mant
Integer	     ii_tipo
String	     is_report, is_nula


end variables

on w_info_predios_cuarteles.create
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
this.st_44=create st_44
this.uo_muestraproductor=create uo_muestraproductor
this.st_5=create st_5
this.st_6=create st_6
this.uo_muestraagronomo=create uo_muestraagronomo
this.uo_muestratipoprod=create uo_muestratipoprod
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
this.Control[iCurrent+10]=this.st_44
this.Control[iCurrent+11]=this.uo_muestraproductor
this.Control[iCurrent+12]=this.st_5
this.Control[iCurrent+13]=this.st_6
this.Control[iCurrent+14]=this.uo_muestraagronomo
this.Control[iCurrent+15]=this.uo_muestratipoprod
end on

on w_info_predios_cuarteles.destroy
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
destroy(this.st_44)
destroy(this.uo_muestraproductor)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.uo_muestraagronomo)
destroy(this.uo_muestratipoprod)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_muestrazona.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_MuestraProductor.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_muestratipoprod.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_MuestraEspecies.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_MuestraVariedad.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_MuestraAgronomo.Codigo) Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_MuestraZona.Seleccion(True, False)
	uo_MuestraProductor.Seleccion(True, False)
	uo_MuestraTipoProd.Seleccion(True, False)
	uo_MuestraEspecies.Seleccion(True, False)
	uo_MuestraVariedad.Seleccion(True, False)
	uo_MuestraAgronomo.Seleccion(True, False)
	
	uo_MuestraProductor.Filtra(0)
	uo_MuestraEspecies.dw_seleccion.Object.codigo[1] = 11
	uo_MuestraEspecies.Codigo = 11
	uo_MuestraVariedad.Filtra(11)
	uo_MuestraAgronomo.Filtra(-1)
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_predios_cuarteles
end type

type st_computador from w_para_informes`st_computador within w_info_predios_cuarteles
end type

type st_usuario from w_para_informes`st_usuario within w_info_predios_cuarteles
end type

type st_temporada from w_para_informes`st_temporada within w_info_predios_cuarteles
end type

type p_logo from w_para_informes`p_logo within w_info_predios_cuarteles
end type

type st_titulo from w_para_informes`st_titulo within w_info_predios_cuarteles
boolean visible = false
integer y = 352
integer width = 1833
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_predios_cuarteles
string tag = "Imprimir Reporte"
integer x = 2281
integer y = 548
integer taborder = 190
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Long     ll_Fila

SetPointer(HourGlass!)

istr_info.titulo	= 'INFORME - PREDIOS CUARTELES'
OpenWithParm(vinf,istr_info)
vinf.dw_1.DataObject = "dw_info_predioscuarteles"
vinf.dw_1.SetTransObject(sqlca)
ll_Fila = vinf.dw_1.Retrieve(uo_MuestraZona.Codigo, uo_MuestraProductor.Codigo, uo_MuestraTipoProd.Codigo,&
									uo_MuestraEspecies.Codigo, uo_MuestraVariedad.Codigo, uo_MuestraAgronomo.Codigo)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
   vinf.dw_1.Modify("t_numero.text = '" + '3' + "'")

	
	vinf.dw_1.Object.DataWindow.Zoom = 98
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_predios_cuarteles
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2281
integer y = 828
integer taborder = 200
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_zona from statictext within w_info_predios_cuarteles
integer x = 379
integer y = 560
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

type st_33 from statictext within w_info_predios_cuarteles
integer x = 379
integer y = 684
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

type st_14 from statictext within w_info_predios_cuarteles
integer x = 379
integer y = 936
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

type st_2 from statictext within w_info_predios_cuarteles
integer x = 1696
integer y = 468
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

type uo_muestrazona from uo_seleccion_zonas_mod within w_info_predios_cuarteles
integer x = 832
integer y = 524
integer width = 1042
integer height = 108
integer taborder = 130
boolean bringtotop = true
end type

on uo_muestrazona.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1
		uo_muestraagronomo.Todos(True)
		uo_muestraagronomo.cbx_Todos.Enabled	=	False
		uo_muestraagronomo.Filtra(This.Codigo)
		
	CASE ELSE
		uo_muestraagronomo.Filtra(This.Codigo)
		uo_muestraagronomo.cbx_Todos.Enabled	=	True
END CHOOSE
end event

type uo_muestraespecies from uo_seleccion_especie_mod within w_info_predios_cuarteles
event destroy ( )
integer x = 832
integer y = 900
integer width = 1056
integer height = 108
integer taborder = 150
boolean bringtotop = true
end type

on uo_muestraespecies.destroy
call uo_seleccion_especie_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1
		uo_muestravariedad.Todos(True)
		uo_muestravariedad.cbx_Todos.Enabled	=	False
				
	CASE ELSE
		uo_muestravariedad.Filtra(This.Codigo)
		uo_muestravariedad.cbx_Todos.Enabled	=	True
		
END CHOOSE
end event

type st_1 from statictext within w_info_predios_cuarteles
integer x = 379
integer y = 812
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

type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_predios_cuarteles
integer x = 832
integer y = 1028
integer width = 1042
integer height = 108
integer taborder = 160
boolean bringtotop = true
end type

on uo_muestravariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type st_3 from statictext within w_info_predios_cuarteles
integer x = 379
integer y = 1068
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

type st_44 from statictext within w_info_predios_cuarteles
integer x = 247
integer y = 440
integer width = 1829
integer height = 920
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

type uo_muestraproductor from uo_seleccion_productor_mod within w_info_predios_cuarteles
event destroy ( )
integer x = 832
integer y = 640
integer width = 1042
integer height = 108
integer taborder = 140
boolean bringtotop = true
end type

on uo_muestraproductor.destroy
call uo_seleccion_productor_mod::destroy
end on

type st_5 from statictext within w_info_predios_cuarteles
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
string text = "Predios Cuarteles"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_predios_cuarteles
integer x = 379
integer y = 1192
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

type uo_muestraagronomo from uo_seleccion_agronomo_mod within w_info_predios_cuarteles
integer x = 832
integer y = 1160
integer width = 1042
integer height = 124
integer taborder = 170
boolean bringtotop = true
end type

on uo_muestraagronomo.destroy
call uo_seleccion_agronomo_mod::destroy
end on

type uo_muestratipoprod from uo_seleccion_tipoproductor_mod within w_info_predios_cuarteles
integer x = 832
integer y = 772
integer width = 1042
integer height = 124
integer taborder = 210
boolean bringtotop = true
end type

on uo_muestratipoprod.destroy
call uo_seleccion_tipoproductor_mod::destroy
end on

