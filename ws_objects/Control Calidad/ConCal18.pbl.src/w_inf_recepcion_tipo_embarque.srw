$PBExportHeader$w_inf_recepcion_tipo_embarque.srw
$PBExportComments$Ventana de Informe de Resumen de palnillas de evaluación de descarte de fruta de packing
forward
global type w_inf_recepcion_tipo_embarque from w_para_informes
end type
type em_fechadesde from editmask within w_inf_recepcion_tipo_embarque
end type
type st_12 from statictext within w_inf_recepcion_tipo_embarque
end type
type st_13 from statictext within w_inf_recepcion_tipo_embarque
end type
type em_fechahasta from editmask within w_inf_recepcion_tipo_embarque
end type
type st_zona from statictext within w_inf_recepcion_tipo_embarque
end type
type st_33 from statictext within w_inf_recepcion_tipo_embarque
end type
type cbx_todosfecha from checkbox within w_inf_recepcion_tipo_embarque
end type
type st_2 from statictext within w_inf_recepcion_tipo_embarque
end type
type uo_selzona from uo_seleccion_zonas_mod within w_inf_recepcion_tipo_embarque
end type
type st_1 from statictext within w_inf_recepcion_tipo_embarque
end type
type st_4 from statictext within w_inf_recepcion_tipo_embarque
end type
type uo_selvariedad from uo_seleccion_variedad_mod within w_inf_recepcion_tipo_embarque
end type
type st_3 from statictext within w_inf_recepcion_tipo_embarque
end type
type uo_selproductor from uo_seleccion_productor_mod within w_inf_recepcion_tipo_embarque
end type
type st_10 from statictext within w_inf_recepcion_tipo_embarque
end type
type rb_planta from radiobutton within w_inf_recepcion_tipo_embarque
end type
type rb_prod from radiobutton within w_inf_recepcion_tipo_embarque
end type
type rb_existencia from radiobutton within w_inf_recepcion_tipo_embarque
end type
type st_44 from statictext within w_inf_recepcion_tipo_embarque
end type
type st_8 from statictext within w_inf_recepcion_tipo_embarque
end type
type uo_selcamara from uo_seleccion_camarasbode_mod within w_inf_recepcion_tipo_embarque
end type
type st_11 from statictext within w_inf_recepcion_tipo_embarque
end type
type uo_selcliente from uo_seleccion_clientesprod within w_inf_recepcion_tipo_embarque
end type
type st_5 from statictext within w_inf_recepcion_tipo_embarque
end type
type st_6 from statictext within w_inf_recepcion_tipo_embarque
end type
type uo_selplanta from uo_seleccion_planta_mod within w_inf_recepcion_tipo_embarque
end type
end forward

global type w_inf_recepcion_tipo_embarque from w_para_informes
string tag = "Consulta Planilla Recepción"
integer x = 14
integer y = 32
integer width = 2638
integer height = 2092
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
em_fechadesde em_fechadesde
st_12 st_12
st_13 st_13
em_fechahasta em_fechahasta
st_zona st_zona
st_33 st_33
cbx_todosfecha cbx_todosfecha
st_2 st_2
uo_selzona uo_selzona
st_1 st_1
st_4 st_4
uo_selvariedad uo_selvariedad
st_3 st_3
uo_selproductor uo_selproductor
st_10 st_10
rb_planta rb_planta
rb_prod rb_prod
rb_existencia rb_existencia
st_44 st_44
st_8 st_8
uo_selcamara uo_selcamara
st_11 st_11
uo_selcliente uo_selcliente
st_5 st_5
st_6 st_6
uo_selplanta uo_selplanta
end type
global w_inf_recepcion_tipo_embarque w_inf_recepcion_tipo_embarque

type variables
str_busqueda	istr_busq
str_mant         istr_mant
uo_Especie		iuo_Especie
uo_SelCamara	iuo_SelCamara

end variables

forward prototypes
public subroutine wf_visible (integer ii_especie)
end prototypes

public subroutine wf_visible (integer ii_especie);IF ii_especie = 41 Then
	rb_prod.Visible			=	False
ElseIf ii_especie = 23 Then
	rb_prod.Visible			=	False
	rb_existencia.Visible	=	False
ElseIf ii_especie = 21 Then
	uo_SelCamara.Visible	= False
	st_8.Visible				= False
	st_44.Height		= st_44.Height - 120
	
	st_10.y			= st_10.y	- 120
	st_12.y			= st_12.y	- 120
	st_13.y			= st_12.y
	st_4.y			= st_4.y		- 120
	em_fechadesde.y	=	em_fechadesde.y -120
	em_fechahasta.y	= em_fechadesde.y
	cbx_todosfecha.y = cbx_todosfecha.y - 120
	
	st_11.y			= st_11.y	- 120
	
	rb_planta.y		=	rb_planta.y	-120
	rb_prod.y		=	rb_planta.y
	rb_existencia.y	=	rb_planta.y
End If
end subroutine

on w_inf_recepcion_tipo_embarque.create
int iCurrent
call super::create
this.em_fechadesde=create em_fechadesde
this.st_12=create st_12
this.st_13=create st_13
this.em_fechahasta=create em_fechahasta
this.st_zona=create st_zona
this.st_33=create st_33
this.cbx_todosfecha=create cbx_todosfecha
this.st_2=create st_2
this.uo_selzona=create uo_selzona
this.st_1=create st_1
this.st_4=create st_4
this.uo_selvariedad=create uo_selvariedad
this.st_3=create st_3
this.uo_selproductor=create uo_selproductor
this.st_10=create st_10
this.rb_planta=create rb_planta
this.rb_prod=create rb_prod
this.rb_existencia=create rb_existencia
this.st_44=create st_44
this.st_8=create st_8
this.uo_selcamara=create uo_selcamara
this.st_11=create st_11
this.uo_selcliente=create uo_selcliente
this.st_5=create st_5
this.st_6=create st_6
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_fechadesde
this.Control[iCurrent+2]=this.st_12
this.Control[iCurrent+3]=this.st_13
this.Control[iCurrent+4]=this.em_fechahasta
this.Control[iCurrent+5]=this.st_zona
this.Control[iCurrent+6]=this.st_33
this.Control[iCurrent+7]=this.cbx_todosfecha
this.Control[iCurrent+8]=this.st_2
this.Control[iCurrent+9]=this.uo_selzona
this.Control[iCurrent+10]=this.st_1
this.Control[iCurrent+11]=this.st_4
this.Control[iCurrent+12]=this.uo_selvariedad
this.Control[iCurrent+13]=this.st_3
this.Control[iCurrent+14]=this.uo_selproductor
this.Control[iCurrent+15]=this.st_10
this.Control[iCurrent+16]=this.rb_planta
this.Control[iCurrent+17]=this.rb_prod
this.Control[iCurrent+18]=this.rb_existencia
this.Control[iCurrent+19]=this.st_44
this.Control[iCurrent+20]=this.st_8
this.Control[iCurrent+21]=this.uo_selcamara
this.Control[iCurrent+22]=this.st_11
this.Control[iCurrent+23]=this.uo_selcliente
this.Control[iCurrent+24]=this.st_5
this.Control[iCurrent+25]=this.st_6
this.Control[iCurrent+26]=this.uo_selplanta
end on

on w_inf_recepcion_tipo_embarque.destroy
call super::destroy
destroy(this.em_fechadesde)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_fechahasta)
destroy(this.st_zona)
destroy(this.st_33)
destroy(this.cbx_todosfecha)
destroy(this.st_2)
destroy(this.uo_selzona)
destroy(this.st_1)
destroy(this.st_4)
destroy(this.uo_selvariedad)
destroy(this.st_3)
destroy(this.uo_selproductor)
destroy(this.st_10)
destroy(this.rb_planta)
destroy(this.rb_prod)
destroy(this.rb_existencia)
destroy(this.st_44)
destroy(this.st_8)
destroy(this.uo_selcamara)
destroy(this.st_11)
destroy(this.uo_selcliente)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.uo_selplanta)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelVariedad.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelZona.Codigo) 		Then lb_Cerrar	=	True
If IsNull(uo_SelPlanta.Codigo) 		Then lb_Cerrar	=	True
If IsNull(uo_SelProductor.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelCamara.Codigo)	Then lb_Cerrar =	True
If IsNull(uo_SelCliente.Codigo)		Then lb_Cerrar =	True

IF lb_Cerrar THEN
	Close(This)
ELSE	
	iuo_Especie	=	Create uo_Especie
	iuo_Especie.Existe(Integer(Message.StringParm), False, Sqlca)

	This.Title	= "RESUMEN RECEPCION " + Upper(iuo_Especie.Nombre)
	
	wf_Visible(iuo_Especie.Codigo)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelZona.Seleccion(True, False)
	uo_SelPlanta.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	uo_SelCamara.Seleccion(True, False)
	uo_SelCliente.Seleccion(True, False)
	uo_SelVariedad.filtra(iuo_Especie.Codigo)
	uo_SelProductor.filtra(0)
	uo_SelPlanta.cbx_todos.checked = True
	uo_SelPlanta.cbx_todos.triggerevent('clicked')
	uo_SelProductor.triggerevent('ue_cambio')
		
	em_FechaDesde.text	  	= String(RelativeDate(Today(), -365))
	em_FechaHasta.text	  	= String(Today())
	em_FechaDesde.SetFocus()
	
	istr_Mant.Argumento[1] 	= '1'
	istr_Mant.Argumento[2] 	= '1'
	istr_Mant.Argumento[3] 	= '1'
	istr_Mant.Argumento[4] 	= '-1'
End If
end event

type pb_excel from w_para_informes`pb_excel within w_inf_recepcion_tipo_embarque
end type

type st_computador from w_para_informes`st_computador within w_inf_recepcion_tipo_embarque
integer x = 1664
end type

type st_usuario from w_para_informes`st_usuario within w_inf_recepcion_tipo_embarque
integer x = 1664
end type

type st_temporada from w_para_informes`st_temporada within w_inf_recepcion_tipo_embarque
integer x = 1664
end type

type p_logo from w_para_informes`p_logo within w_inf_recepcion_tipo_embarque
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_inf_recepcion_tipo_embarque
integer width = 1824
boolean enabled = true
string text = "Detalle Recepcion"
end type

type pb_acepta from w_para_informes`pb_acepta within w_inf_recepcion_tipo_embarque
string tag = "Imprimir Reporte"
integer x = 2181
integer y = 556
integer taborder = 160
end type

event pb_acepta::clicked;Integer	li_fila, li_brix, li_firmeza, li_exporta, li_embarque
Date		ld_FechaEmbaini, ld_FechaEmbafin

//Exporta
If IsNull(istr_Mant.Argumento[3])  Then
	MessageBox("Atención","Debe Seleccionar Porcentaje Exportación Previamente",Exclamation!)
	Return
End If

//Embarque
If iuo_Especie.Codigo = 41 Then
	li_embarque = -1
Else
	li_embarque	= Integer(istr_Mant.Argumento[4])
	If IsNull(li_embarque) OR li_embarque=0 Then
		MessageBox("Atención","Debe Seleccionar Embarque Previamente",Exclamation!)
		Return
	End If		
End If

//Fecha Recepción
If cbx_todosfecha.Checked Then
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
Else
	ld_FechaEmbaini = Date(em_fechadesde.Text)
	ld_FechaEmbafin = Date(em_fechahasta.Text)
End If

OpenWithParm(vinf, istr_info)
istr_info.titulo = 'RESUMEN RECEPCION ' + Upper(iuo_Especie.Nombre)

If iuo_Especie.Codigo = 21 Then//Cerezas
	If rb_planta.Checked Then
		vinf.dw_1.DataObject = "dw_inf_recepcion_tipo_embarque"
		vinf.dw_1.Object.DataWindow.Zoom = 88
	ElseIf rb_Existencia.Checked Then
		vinf.dw_1.DataObject = "dw_inf_recepcion_tipo_embarque_ex"
		vinf.dw_1.Object.DataWindow.Zoom = 80
	Else
		vinf.dw_1.DataObject = "dw_inf_recepcion_tipo_embarque_pr"
		vinf.dw_1.Object.DataWindow.Zoom = 86
	End If	
ElseIf iuo_Especie.Codigo = 23 Then//Ciruelas
	vinf.dw_1.DataObject = "dw_inf_recepcion_ciruelas"
//	vinf.dw_1.Object.DataWindow.Zoom = 85
ElseIf iuo_Especie.Codigo = 41 Then
	If rb_planta.Checked Then
		vinf.dw_1.DataObject = "dw_infrecrecepcion_kiwis"
		vinf.dw_1.Object.DataWindow.Zoom = 75
	ElseIf rb_Existencia.Checked Then
		vinf.dw_1.DataObject = "dw_infrecrecepcion_kiwis_exis"
		vinf.dw_1.Object.DataWindow.Zoom = 75
	End If
End If

vinf.dw_1.SetTransObject(sqlca)

If iuo_Especie.Codigo = 21 Or iuo_Especie.Codigo = 23 Then
	li_fila = vinf.dw_1.Retrieve(uo_SelVariedad.Codigo, uo_SelZona.Codigo, uo_SelPlanta.Codigo, uo_Selproductor.Codigo, -1,&
		 Integer(istr_Mant.Argumento[3]),ld_FechaEmbaini,ld_FechaEmbafin,&
		 Integer(istr_Mant.Argumento[1]),Integer(istr_Mant.Argumento[2]),li_embarque, iuo_Especie.Codigo, uo_SelCliente.Codigo)
Elseif iuo_Especie.Codigo = 41 Then
	li_fila = vinf.dw_1.Retrieve(uo_SelVariedad.Codigo, uo_SelZona.Codigo, uo_SelPlanta.Codigo, uo_Selproductor.Codigo, -1,&
		  Integer(istr_Mant.Argumento[3]),ld_FechaEmbaini,ld_FechaEmbafin,&
		  Integer(istr_Mant.Argumento[1]),Integer(istr_Mant.Argumento[2]),li_embarque, iuo_Especie.Codigo,uo_SelCamara.Codigo,&
		  uo_SelCliente.Codigo)
End If
									
If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy("desde.text = '" + em_FechaDesde.text + "'")
	vinf.dw_1.ModIfy("hasta.text = '" + em_FechaHasta.text + "'")	
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

type pb_salir from w_para_informes`pb_salir within w_inf_recepcion_tipo_embarque
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2171
integer y = 888
integer taborder = 170
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type em_fechadesde from editmask within w_inf_recepcion_tipo_embarque
integer x = 741
integer y = 1520
integer width = 489
integer height = 92
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_12 from statictext within w_inf_recepcion_tipo_embarque
integer x = 782
integer y = 1464
integer width = 407
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
boolean enabled = false
string text = "Desde"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_13 from statictext within w_inf_recepcion_tipo_embarque
integer x = 1321
integer y = 1464
integer width = 407
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
boolean enabled = false
string text = "Hasta"
alignment alignment = center!
boolean focusrectangle = false
end type

type em_fechahasta from editmask within w_inf_recepcion_tipo_embarque
integer x = 1280
integer y = 1520
integer width = 489
integer height = 92
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_zona from statictext within w_inf_recepcion_tipo_embarque
integer x = 347
integer y = 916
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
string text = "Zona Origen"
boolean focusrectangle = false
end type

type st_33 from statictext within w_inf_recepcion_tipo_embarque
integer x = 343
integer y = 1156
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

type cbx_todosfecha from checkbox within w_inf_recepcion_tipo_embarque
integer x = 1838
integer y = 1532
integer width = 123
integer height = 64
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 553648127
string text = " "
end type

event clicked;If This.Checked Then
	em_fechadesde.Enabled	=	False
	em_fechahasta.Enabled	=	False
	em_fechadesde.Text 		= '01/01/2000'
	em_fechahasta.Text 		= String(Today())
Else
	em_fechadesde.Enabled	=	True
	em_fechahasta.Enabled	=	True
	em_FechaDesde.text	  	= String(RelativeDate(Today(), -365))
	em_FechaHasta.text	  	= String(Today())
	em_fechadesde.SetFocus()	
End If


end event

type st_2 from statictext within w_inf_recepcion_tipo_embarque
integer x = 1778
integer y = 728
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

type uo_selzona from uo_seleccion_zonas_mod within w_inf_recepcion_tipo_embarque
integer x = 896
integer y = 896
integer width = 1083
integer height = 112
integer taborder = 20
boolean bringtotop = true
end type

on uo_selzona.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

Choose Case This.Codigo
	Case -1, -9
		uo_SelPlanta.Todos(True)		
		
	Case Else
		
End Choose
end event

type st_1 from statictext within w_inf_recepcion_tipo_embarque
integer x = 347
integer y = 1036
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
string text = "Frigorífico"
boolean focusrectangle = false
end type

type st_4 from statictext within w_inf_recepcion_tipo_embarque
integer x = 320
integer y = 1532
integer width = 375
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
string text = "Recepción"
boolean focusrectangle = false
end type

type uo_selvariedad from uo_seleccion_variedad_mod within w_inf_recepcion_tipo_embarque
integer x = 896
integer y = 764
integer width = 1083
integer height = 112
integer taborder = 10
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type st_3 from statictext within w_inf_recepcion_tipo_embarque
integer x = 347
integer y = 780
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
string text = "Variedad"
boolean focusrectangle = false
end type

type uo_selproductor from uo_seleccion_productor_mod within w_inf_recepcion_tipo_embarque
event destroy ( )
integer x = 896
integer y = 1136
integer width = 1083
integer height = 116
integer taborder = 40
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor_mod::destroy
end on

event ue_cambio;call super::ue_cambio;
If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		this.dw_Seleccion.Object.codigo.BackGround.Color	=	553648127
	Case Else
		this.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)	
End Choose
end event

type st_10 from statictext within w_inf_recepcion_tipo_embarque
integer x = 251
integer y = 1436
integer width = 1824
integer height = 256
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_planta from radiobutton within w_inf_recepcion_tipo_embarque
integer x = 334
integer y = 1780
integer width = 663
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Total Recepcionado"
boolean checked = true
boolean lefttext = true
end type

type rb_prod from radiobutton within w_inf_recepcion_tipo_embarque
integer x = 1056
integer y = 1780
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
boolean lefttext = true
end type

type rb_existencia from radiobutton within w_inf_recepcion_tipo_embarque
integer x = 1531
integer y = 1780
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Existencia"
boolean lefttext = true
end type

type st_44 from statictext within w_inf_recepcion_tipo_embarque
integer x = 251
integer y = 664
integer width = 1824
integer height = 772
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_8 from statictext within w_inf_recepcion_tipo_embarque
integer x = 347
integer y = 1276
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
string text = "Cámara"
boolean focusrectangle = false
end type

type uo_selcamara from uo_seleccion_camarasbode_mod within w_inf_recepcion_tipo_embarque
integer x = 896
integer y = 1256
integer width = 1083
integer taborder = 50
boolean bringtotop = true
end type

on uo_selcamara.destroy
call uo_seleccion_camarasbode_mod::destroy
end on

type st_11 from statictext within w_inf_recepcion_tipo_embarque
integer x = 251
integer y = 1692
integer width = 1824
integer height = 256
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_inf_recepcion_tipo_embarque
integer x = 896
integer y = 452
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_5 from statictext within w_inf_recepcion_tipo_embarque
integer x = 251
integer y = 412
integer width = 1824
integer height = 256
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_inf_recepcion_tipo_embarque
integer x = 375
integer y = 532
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
string text = "Clliente"
boolean focusrectangle = false
end type

type uo_selplanta from uo_seleccion_planta_mod within w_inf_recepcion_tipo_embarque
integer x = 896
integer y = 1020
integer width = 1083
integer height = 112
integer taborder = 60
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_planta_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

//Choose Case This.Codigo
//	Case -1, -9
//		this.dw_Seleccion.Object.codigo.BackGround.Color	=	553648127
//		uo_selcamara.enabled = False
//		uo_selcamara.todos(True)
//	Case Else
//		uo_SelCamara.Filtra2(This.Codigo)
//		uo_selcamara.enabled = True
//		this.dw_Seleccion.Object.codigo.BackGround.Color	=	RGB(255, 255, 255)	
//End Choose
end event

