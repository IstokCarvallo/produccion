$PBExportHeader$w_info_resumen_certificaciones_plde.srw
$PBExportComments$Ventana Informe de Certificacion Planta.
forward
global type w_info_resumen_certificaciones_plde from w_para_informes
end type
type st_33 from statictext within w_info_resumen_certificaciones_plde
end type
type st_44 from statictext within w_info_resumen_certificaciones_plde
end type
type st_5 from statictext within w_info_resumen_certificaciones_plde
end type
type st_1 from statictext within w_info_resumen_certificaciones_plde
end type
type st_2 from statictext within w_info_resumen_certificaciones_plde
end type
type uo_selzonas from uo_seleccion_zonas_mod within w_info_resumen_certificaciones_plde
end type
type uo_selprotocolo from uo_seleccion_protocolo_mod within w_info_resumen_certificaciones_plde
end type
type uo_selplanta from uo_seleccion_plantapacking_mod within w_info_resumen_certificaciones_plde
end type
type cbx_cross from checkbox within w_info_resumen_certificaciones_plde
end type
type st_3 from statictext within w_info_resumen_certificaciones_plde
end type
type uo_selespecie from uo_seleccion_especie_mod within w_info_resumen_certificaciones_plde
end type
type uo_selestado from uo_seleccion_prodestadocert_mod within w_info_resumen_certificaciones_plde
end type
type st_4 from statictext within w_info_resumen_certificaciones_plde
end type
end forward

global type w_info_resumen_certificaciones_plde from w_para_informes
integer x = 14
integer y = 32
integer width = 2469
integer height = 1568
string icon = "AppIcon!"
st_33 st_33
st_44 st_44
st_5 st_5
st_1 st_1
st_2 st_2
uo_selzonas uo_selzonas
uo_selprotocolo uo_selprotocolo
uo_selplanta uo_selplanta
cbx_cross cbx_cross
st_3 st_3
uo_selespecie uo_selespecie
uo_selestado uo_selestado
st_4 st_4
end type
global w_info_resumen_certificaciones_plde w_info_resumen_certificaciones_plde

type variables
str_busqueda istr_busq
str_mant istr_mant

Integer	ii_tipo, codigo
String		is_report, nombre

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_zona,idwc_especie


uo_especie				iuo_especie
uo_zonas				iuo_zonas
end variables

on w_info_resumen_certificaciones_plde.create
int iCurrent
call super::create
this.st_33=create st_33
this.st_44=create st_44
this.st_5=create st_5
this.st_1=create st_1
this.st_2=create st_2
this.uo_selzonas=create uo_selzonas
this.uo_selprotocolo=create uo_selprotocolo
this.uo_selplanta=create uo_selplanta
this.cbx_cross=create cbx_cross
this.st_3=create st_3
this.uo_selespecie=create uo_selespecie
this.uo_selestado=create uo_selestado
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_33
this.Control[iCurrent+2]=this.st_44
this.Control[iCurrent+3]=this.st_5
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.uo_selzonas
this.Control[iCurrent+7]=this.uo_selprotocolo
this.Control[iCurrent+8]=this.uo_selplanta
this.Control[iCurrent+9]=this.cbx_cross
this.Control[iCurrent+10]=this.st_3
this.Control[iCurrent+11]=this.uo_selespecie
this.Control[iCurrent+12]=this.uo_selestado
this.Control[iCurrent+13]=this.st_4
end on

on w_info_resumen_certificaciones_plde.destroy
call super::destroy
destroy(this.st_33)
destroy(this.st_44)
destroy(this.st_5)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.uo_selzonas)
destroy(this.uo_selprotocolo)
destroy(this.uo_selplanta)
destroy(this.cbx_cross)
destroy(this.st_3)
destroy(this.uo_selespecie)
destroy(this.uo_selestado)
destroy(this.st_4)
end on

event open;Boolean	lb_Cerrar

If IsNull(uo_SelEspecie.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelPlanta.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelZonas.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelProtocolo.Codigo)	Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else 
	uo_SelEspecie.Seleccion(True, False)
	uo_SelPlanta.Seleccion(True, False)
	uo_SelZonas.Seleccion(True, False)
	uo_SelProtocolo.Seleccion(True, False)

End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_resumen_certificaciones_plde
end type

type st_computador from w_para_informes`st_computador within w_info_resumen_certificaciones_plde
end type

type st_usuario from w_para_informes`st_usuario within w_info_resumen_certificaciones_plde
end type

type st_temporada from w_para_informes`st_temporada within w_info_resumen_certificaciones_plde
end type

type p_logo from w_para_informes`p_logo within w_info_resumen_certificaciones_plde
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_resumen_certificaciones_plde
integer width = 1527
string text = "Resumen Certificaciones Plantas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resumen_certificaciones_plde
string tag = "Imprimir Reporte"
integer x = 1957
integer y = 552
integer taborder = 110
end type

event pb_acepta::clicked;Integer	li_fila

SetPointer(Arrow!)

istr_info.titulo	= 'RESUMEN CERTIFICACION PLANTAS'

OpenWithParm(vinf,istr_info)

If cbx_cross.Checked Then
	vinf.dw_1.DataObject = "dw_info_resumen_certificacion_plde"
Else
	vinf.dw_1.DataObject = "dw_info_cert_plantas"
End If
	
vinf.dw_1.SetTransObject(sqlca)

li_fila	=	vinf.dw_1.Retrieve(uo_SelProtocolo.Codigo, uo_SelZonas.Codigo, uo_SelPlanta.Codigo, uo_SelEspecie.Codigo, uo_SelEstado.Codigo)
									  
IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
		
	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_resumen_certificaciones_plde
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1957
integer y = 832
integer taborder = 120
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_33 from statictext within w_info_resumen_certificaciones_plde
integer x = 352
integer y = 948
integer width = 325
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Zona"
boolean focusrectangle = false
end type

type st_44 from statictext within w_info_resumen_certificaciones_plde
integer x = 251
integer y = 440
integer width = 1527
integer height = 836
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_resumen_certificaciones_plde
integer x = 1518
integer y = 456
integer width = 201
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
string text = "Todos"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_resumen_certificaciones_plde
integer x = 352
integer y = 564
integer width = 325
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_resumen_certificaciones_plde
integer x = 352
integer y = 820
integer width = 325
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type uo_selzonas from uo_seleccion_zonas_mod within w_info_resumen_certificaciones_plde
integer x = 667
integer y = 924
integer width = 1015
integer height = 132
integer taborder = 130
boolean bringtotop = true
end type

on uo_selzonas.destroy
call uo_seleccion_zonas_mod::destroy
end on

type uo_selprotocolo from uo_seleccion_protocolo_mod within w_info_resumen_certificaciones_plde
integer x = 667
integer y = 668
integer width = 1015
integer taborder = 120
boolean bringtotop = true
long backcolor = 553648127
end type

on uo_selprotocolo.destroy
call uo_seleccion_protocolo_mod::destroy
end on

type uo_selplanta from uo_seleccion_plantapacking_mod within w_info_resumen_certificaciones_plde
integer x = 667
integer y = 796
integer width = 1015
integer taborder = 120
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantapacking_mod::destroy
end on

type cbx_cross from checkbox within w_info_resumen_certificaciones_plde
integer x = 1280
integer y = 1184
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
string text = "CrossTab"
end type

type st_3 from statictext within w_info_resumen_certificaciones_plde
integer x = 352
integer y = 692
integer width = 325
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Protocolo"
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie_mod within w_info_resumen_certificaciones_plde
integer x = 667
integer y = 540
integer width = 1015
integer taborder = 120
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie_mod::destroy
end on

type uo_selestado from uo_seleccion_prodestadocert_mod within w_info_resumen_certificaciones_plde
integer x = 667
integer y = 1056
integer width = 1015
integer taborder = 140
boolean bringtotop = true
long backcolor = 553648127
end type

on uo_selestado.destroy
call uo_seleccion_prodestadocert_mod::destroy
end on

type st_4 from statictext within w_info_resumen_certificaciones_plde
integer x = 352
integer y = 1076
integer width = 325
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Estado"
boolean focusrectangle = false
end type

