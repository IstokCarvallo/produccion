$PBExportHeader$w_info_resumen_certificaciones_prod.srw
$PBExportComments$Ventana Informe de Certificacion Productor.
forward
global type w_info_resumen_certificaciones_prod from w_para_informes
end type
type st_33 from statictext within w_info_resumen_certificaciones_prod
end type
type st_5 from statictext within w_info_resumen_certificaciones_prod
end type
type st_1 from statictext within w_info_resumen_certificaciones_prod
end type
type uo_selzonas from uo_seleccion_zonas_mod within w_info_resumen_certificaciones_prod
end type
type uo_selprotocolo from uo_seleccion_protocolo_mod within w_info_resumen_certificaciones_prod
end type
type st_2 from statictext within w_info_resumen_certificaciones_prod
end type
type st_3 from statictext within w_info_resumen_certificaciones_prod
end type
type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_resumen_certificaciones_prod
end type
type uo_selpredio from uo_seleccion_predioproduc_mod within w_info_resumen_certificaciones_prod
end type
type st_4 from statictext within w_info_resumen_certificaciones_prod
end type
type uo_selespecie from uo_seleccion_especie_mod within w_info_resumen_certificaciones_prod
end type
type st_6 from statictext within w_info_resumen_certificaciones_prod
end type
type gb_3 from groupbox within w_info_resumen_certificaciones_prod
end type
type st_44 from statictext within w_info_resumen_certificaciones_prod
end type
type rb_protocolo from radiobutton within w_info_resumen_certificaciones_prod
end type
type rb_zona from radiobutton within w_info_resumen_certificaciones_prod
end type
type rb_especie from radiobutton within w_info_resumen_certificaciones_prod
end type
type st_7 from statictext within w_info_resumen_certificaciones_prod
end type
type uo_selestado from uo_seleccion_prodestadocert_mod within w_info_resumen_certificaciones_prod
end type
end forward

global type w_info_resumen_certificaciones_prod from w_para_informes
integer x = 14
integer y = 32
integer width = 2373
integer height = 1828
string icon = "AppIcon!"
st_33 st_33
st_5 st_5
st_1 st_1
uo_selzonas uo_selzonas
uo_selprotocolo uo_selprotocolo
st_2 st_2
st_3 st_3
uo_selproductor uo_selproductor
uo_selpredio uo_selpredio
st_4 st_4
uo_selespecie uo_selespecie
st_6 st_6
gb_3 gb_3
st_44 st_44
rb_protocolo rb_protocolo
rb_zona rb_zona
rb_especie rb_especie
st_7 st_7
uo_selestado uo_selestado
end type
global w_info_resumen_certificaciones_prod w_info_resumen_certificaciones_prod

type variables
str_busqueda istr_busq
str_mant istr_mant

Integer	ii_tipo, codigo
String		is_report, nombre

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta, idwc_zona,idwc_especie


uo_especie				iuo_especie
uo_zonas				iuo_zonas
end variables

on w_info_resumen_certificaciones_prod.create
int iCurrent
call super::create
this.st_33=create st_33
this.st_5=create st_5
this.st_1=create st_1
this.uo_selzonas=create uo_selzonas
this.uo_selprotocolo=create uo_selprotocolo
this.st_2=create st_2
this.st_3=create st_3
this.uo_selproductor=create uo_selproductor
this.uo_selpredio=create uo_selpredio
this.st_4=create st_4
this.uo_selespecie=create uo_selespecie
this.st_6=create st_6
this.gb_3=create gb_3
this.st_44=create st_44
this.rb_protocolo=create rb_protocolo
this.rb_zona=create rb_zona
this.rb_especie=create rb_especie
this.st_7=create st_7
this.uo_selestado=create uo_selestado
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_33
this.Control[iCurrent+2]=this.st_5
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.uo_selzonas
this.Control[iCurrent+5]=this.uo_selprotocolo
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.uo_selproductor
this.Control[iCurrent+9]=this.uo_selpredio
this.Control[iCurrent+10]=this.st_4
this.Control[iCurrent+11]=this.uo_selespecie
this.Control[iCurrent+12]=this.st_6
this.Control[iCurrent+13]=this.gb_3
this.Control[iCurrent+14]=this.st_44
this.Control[iCurrent+15]=this.rb_protocolo
this.Control[iCurrent+16]=this.rb_zona
this.Control[iCurrent+17]=this.rb_especie
this.Control[iCurrent+18]=this.st_7
this.Control[iCurrent+19]=this.uo_selestado
end on

on w_info_resumen_certificaciones_prod.destroy
call super::destroy
destroy(this.st_33)
destroy(this.st_5)
destroy(this.st_1)
destroy(this.uo_selzonas)
destroy(this.uo_selprotocolo)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.uo_selproductor)
destroy(this.uo_selpredio)
destroy(this.st_4)
destroy(this.uo_selespecie)
destroy(this.st_6)
destroy(this.gb_3)
destroy(this.st_44)
destroy(this.rb_protocolo)
destroy(this.rb_zona)
destroy(this.rb_especie)
destroy(this.st_7)
destroy(this.uo_selestado)
end on

event open;Boolean	lb_Cerrar

If IsNull(uo_SelEspecie.Codigo)		Then lb_Cerrar	=	True
If IsNull(uo_SelProductor.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelZonas.Codigo)		Then lb_Cerrar	=	True
If IsNull(uo_SelProtocolo.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelPredio.Codigo)		Then lb_Cerrar	=	True
If IsNull(uo_SelEstado.Codigo)		Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else 
	uo_SelEspecie.Seleccion(True, False)
	uo_SelZonas.Seleccion(True, False)
	uo_SelProtocolo.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	uo_SelPredio.Seleccion(True, False)
	uo_SelPredio.Todos(True)
	uo_SelEstado.Seleccion(True,False)

End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_resumen_certificaciones_prod
end type

type st_computador from w_para_informes`st_computador within w_info_resumen_certificaciones_prod
end type

type st_usuario from w_para_informes`st_usuario within w_info_resumen_certificaciones_prod
end type

type st_temporada from w_para_informes`st_temporada within w_info_resumen_certificaciones_prod
end type

type p_logo from w_para_informes`p_logo within w_info_resumen_certificaciones_prod
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_resumen_certificaciones_prod
integer width = 1595
string text = "Resumen Certificaciones Productor"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resumen_certificaciones_prod
string tag = "Imprimir Reporte"
integer x = 2002
integer y = 516
integer taborder = 110
end type

event pb_acepta::clicked;Integer	li_fila

SetPointer(Arrow!)

istr_info.titulo	= 'RESUMEN CERTIFICACION PRODUCTOR'

OpenWithParm(vinf,istr_info)

If rb_Protocolo.Checked Then
	vinf.dw_1.DataObject = "dw_info_cert_productor"
ElseIf rb_Zona.Checked Then
	vinf.dw_1.DataObject = "dw_info_resumencert_zona"
ElseIf rb_Especie.Checked Then
	vinf.dw_1.DataObject = "dw_info_resumencert_especie"
End If

vinf.dw_1.SetTransObject(sqlca)

li_fila	=	vinf.dw_1.Retrieve(uo_SelProtocolo.Codigo, uo_SelZonas.Codigo, uo_SelPredio.Codigo, uo_SelProductor.Codigo, uo_SelEspecie.Codigo, uo_SelEstado.Codigo)
									  
IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	vinf.dw_1.Modify('DataWindow.Zoom = 75')
		
	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_resumen_certificaciones_prod
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2002
integer y = 800
integer taborder = 120
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_33 from statictext within w_info_resumen_certificaciones_prod
integer x = 338
integer y = 1052
integer width = 357
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
string text = "Predio"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_resumen_certificaciones_prod
integer x = 1559
integer y = 452
integer width = 201
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Todos"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_resumen_certificaciones_prod
integer x = 338
integer y = 924
integer width = 357
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
string text = "Productor"
boolean focusrectangle = false
end type

type uo_selzonas from uo_seleccion_zonas_mod within w_info_resumen_certificaciones_prod
integer x = 709
integer y = 768
integer width = 1019
integer taborder = 130
boolean bringtotop = true
long backcolor = 33543637
end type

on uo_selzonas.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelProductor.Filtra(-1)
	Case Else
		uo_SelProductor.Filtra(This.Codigo)
		
End Choose
end event

type uo_selprotocolo from uo_seleccion_protocolo_mod within w_info_resumen_certificaciones_prod
integer x = 709
integer y = 636
integer width = 987
integer height = 112
integer taborder = 120
boolean bringtotop = true
end type

on uo_selprotocolo.destroy
call uo_seleccion_protocolo_mod::destroy
end on

type st_2 from statictext within w_info_resumen_certificaciones_prod
integer x = 338
integer y = 668
integer width = 357
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

type st_3 from statictext within w_info_resumen_certificaciones_prod
integer x = 338
integer y = 796
integer width = 357
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

type uo_selproductor from uo_seleccion_productor_zonas_mod within w_info_resumen_certificaciones_prod
integer x = 709
integer y = 900
integer width = 1019
integer taborder = 130
boolean bringtotop = true
long backcolor = 33543637
end type

on uo_selproductor.destroy
call uo_seleccion_productor_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelPredio.Filtra(-1)
	Case Else
		uo_SelPredio.Filtra(This.Codigo)
		
End Choose
end event

type uo_selpredio from uo_seleccion_predioproduc_mod within w_info_resumen_certificaciones_prod
integer x = 709
integer y = 1028
integer width = 1019
integer taborder = 140
boolean bringtotop = true
long backcolor = 33543637
end type

on uo_selpredio.destroy
call uo_seleccion_predioproduc_mod::destroy
end on

type st_4 from statictext within w_info_resumen_certificaciones_prod
integer x = 343
integer y = 540
integer width = 357
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

type uo_selespecie from uo_seleccion_especie_mod within w_info_resumen_certificaciones_prod
integer x = 709
integer y = 508
integer width = 1019
integer taborder = 120
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie_mod::destroy
end on

type st_6 from statictext within w_info_resumen_certificaciones_prod
integer x = 251
integer y = 444
integer width = 1595
integer height = 864
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

type gb_3 from groupbox within w_info_resumen_certificaciones_prod
integer x = 306
integer y = 1340
integer width = 1499
integer height = 272
integer taborder = 150
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 16777215
long backcolor = 553648127
string text = " Ordenamiento "
end type

type st_44 from statictext within w_info_resumen_certificaciones_prod
integer x = 251
integer y = 1308
integer width = 1595
integer height = 340
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

type rb_protocolo from radiobutton within w_info_resumen_certificaciones_prod
integer x = 402
integer y = 1440
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 16777215
long backcolor = 553648127
string text = "Protocolo"
boolean checked = true
end type

type rb_zona from radiobutton within w_info_resumen_certificaciones_prod
integer x = 850
integer y = 1440
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 16777215
long backcolor = 553648127
string text = "Zona"
end type

type rb_especie from radiobutton within w_info_resumen_certificaciones_prod
integer x = 1294
integer y = 1440
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
end type

type st_7 from statictext within w_info_resumen_certificaciones_prod
integer x = 338
integer y = 1172
integer width = 357
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

type uo_selestado from uo_seleccion_prodestadocert_mod within w_info_resumen_certificaciones_prod
integer x = 709
integer y = 1156
integer width = 1019
integer taborder = 150
boolean bringtotop = true
end type

on uo_selestado.destroy
call uo_seleccion_prodestadocert_mod::destroy
end on

