$PBExportHeader$w_info_consultareclamos.srw
$PBExportComments$Ventana de consulta planilla de madurez
forward
global type w_info_consultareclamos from w_para_informes
end type
type st_1 from statictext within w_info_consultareclamos
end type
type st_4 from statictext within w_info_consultareclamos
end type
type st_6 from statictext within w_info_consultareclamos
end type
type cbx_planilla from checkbox within w_info_consultareclamos
end type
type em_planilla from editmask within w_info_consultareclamos
end type
type uo_selespecie from uo_seleccion_especie_mod within w_info_consultareclamos
end type
type gb_3 from groupbox within w_info_consultareclamos
end type
type st_2 from statictext within w_info_consultareclamos
end type
type st_3 from statictext within w_info_consultareclamos
end type
type em_desde from editmask within w_info_consultareclamos
end type
type em_hasta from editmask within w_info_consultareclamos
end type
type cbx_fecha from checkbox within w_info_consultareclamos
end type
type uo_selrecibidor from uo_seleccion_recibidor_mod within w_info_consultareclamos
end type
type uo_selproductor from uo_seleccion_productor_mod within w_info_consultareclamos
end type
type uo_selvariedad from uo_seleccion_variedad_mod within w_info_consultareclamos
end type
type st_5 from statictext within w_info_consultareclamos
end type
type st_7 from statictext within w_info_consultareclamos
end type
type st_8 from statictext within w_info_consultareclamos
end type
type em_semana from editmask within w_info_consultareclamos
end type
type st_9 from statictext within w_info_consultareclamos
end type
type st_10 from statictext within w_info_consultareclamos
end type
type cbx_semana from checkbox within w_info_consultareclamos
end type
type dw_1 from uo_dw within w_info_consultareclamos
end type
type dw_2 from uo_dw within w_info_consultareclamos
end type
type dw_3 from uo_dw within w_info_consultareclamos
end type
type st_11 from statictext within w_info_consultareclamos
end type
type st_44 from statictext within w_info_consultareclamos
end type
type rb_productor from radiobutton within w_info_consultareclamos
end type
type rb_exportadora from radiobutton within w_info_consultareclamos
end type
type dw_4 from uo_dw within w_info_consultareclamos
end type
end forward

global type w_info_consultareclamos from w_para_informes
integer x = 14
integer y = 32
integer width = 2400
integer height = 2048
string icon = "AppIcon!"
st_1 st_1
st_4 st_4
st_6 st_6
cbx_planilla cbx_planilla
em_planilla em_planilla
uo_selespecie uo_selespecie
gb_3 gb_3
st_2 st_2
st_3 st_3
em_desde em_desde
em_hasta em_hasta
cbx_fecha cbx_fecha
uo_selrecibidor uo_selrecibidor
uo_selproductor uo_selproductor
uo_selvariedad uo_selvariedad
st_5 st_5
st_7 st_7
st_8 st_8
em_semana em_semana
st_9 st_9
st_10 st_10
cbx_semana cbx_semana
dw_1 dw_1
dw_2 dw_2
dw_3 dw_3
st_11 st_11
st_44 st_44
rb_productor rb_productor
rb_exportadora rb_exportadora
dw_4 dw_4
end type
global w_info_consultareclamos w_info_consultareclamos

type variables
uo_Reclamos	iuo_Reclamos
end variables

forward prototypes
public function boolean wf_carga_imagenes (datawindow adw)
end prototypes

public function boolean wf_carga_imagenes (datawindow adw);Boolean	lb_Retorno = True
Long		ll_Fila, ll_New

adw.Insertrow(0)

For ll_Fila = 1 To 15
	If ll_Fila <= dw_3.RowCount() Then
		If FileExists(dw_3.Object.reid_ruta[ll_Fila] + dw_3.Object.reid_archiv[ll_Fila]) Then
			adw.Modify('p_' + String(ll_Fila) + ".FileName = '" + dw_3.Object.reid_ruta[ll_Fila] + dw_3.Object.reid_archiv[ll_Fila] + "'")
		Else
			adw.Modify('p_' + String(ll_Fila) + ".FileName = '" + iuo_Reclamos.generadocumento(dw_3, ll_Fila, Sqlca) + "'")
		End if
	Else
		adw.Modify('p_' + String(ll_Fila) + '.FileName = ""')
	End If
Next

Return lb_Retorno
end function

on w_info_consultareclamos.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_4=create st_4
this.st_6=create st_6
this.cbx_planilla=create cbx_planilla
this.em_planilla=create em_planilla
this.uo_selespecie=create uo_selespecie
this.gb_3=create gb_3
this.st_2=create st_2
this.st_3=create st_3
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.cbx_fecha=create cbx_fecha
this.uo_selrecibidor=create uo_selrecibidor
this.uo_selproductor=create uo_selproductor
this.uo_selvariedad=create uo_selvariedad
this.st_5=create st_5
this.st_7=create st_7
this.st_8=create st_8
this.em_semana=create em_semana
this.st_9=create st_9
this.st_10=create st_10
this.cbx_semana=create cbx_semana
this.dw_1=create dw_1
this.dw_2=create dw_2
this.dw_3=create dw_3
this.st_11=create st_11
this.st_44=create st_44
this.rb_productor=create rb_productor
this.rb_exportadora=create rb_exportadora
this.dw_4=create dw_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_4
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.cbx_planilla
this.Control[iCurrent+5]=this.em_planilla
this.Control[iCurrent+6]=this.uo_selespecie
this.Control[iCurrent+7]=this.gb_3
this.Control[iCurrent+8]=this.st_2
this.Control[iCurrent+9]=this.st_3
this.Control[iCurrent+10]=this.em_desde
this.Control[iCurrent+11]=this.em_hasta
this.Control[iCurrent+12]=this.cbx_fecha
this.Control[iCurrent+13]=this.uo_selrecibidor
this.Control[iCurrent+14]=this.uo_selproductor
this.Control[iCurrent+15]=this.uo_selvariedad
this.Control[iCurrent+16]=this.st_5
this.Control[iCurrent+17]=this.st_7
this.Control[iCurrent+18]=this.st_8
this.Control[iCurrent+19]=this.em_semana
this.Control[iCurrent+20]=this.st_9
this.Control[iCurrent+21]=this.st_10
this.Control[iCurrent+22]=this.cbx_semana
this.Control[iCurrent+23]=this.dw_1
this.Control[iCurrent+24]=this.dw_2
this.Control[iCurrent+25]=this.dw_3
this.Control[iCurrent+26]=this.st_11
this.Control[iCurrent+27]=this.st_44
this.Control[iCurrent+28]=this.rb_productor
this.Control[iCurrent+29]=this.rb_exportadora
this.Control[iCurrent+30]=this.dw_4
end on

on w_info_consultareclamos.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_4)
destroy(this.st_6)
destroy(this.cbx_planilla)
destroy(this.em_planilla)
destroy(this.uo_selespecie)
destroy(this.gb_3)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.cbx_fecha)
destroy(this.uo_selrecibidor)
destroy(this.uo_selproductor)
destroy(this.uo_selvariedad)
destroy(this.st_5)
destroy(this.st_7)
destroy(this.st_8)
destroy(this.em_semana)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.cbx_semana)
destroy(this.dw_1)
destroy(this.dw_2)
destroy(this.dw_3)
destroy(this.st_11)
destroy(this.st_44)
destroy(this.rb_productor)
destroy(this.rb_exportadora)
destroy(this.dw_4)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelEspecie.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_SelProductor.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_SelVariedad.Codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_Selrecibidor.Codigo) 	Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else	
	uo_SelEspecie.Seleccion(False, False)	
	uo_SelProductor.Seleccion(True, False)
	uo_SelVariedad.Seleccion(True, False)
	uo_SelRecibidor.Seleccion(True, False)
	
	uo_SelEspecie.Codigo = 11
	uo_SelEspecie.dw_Seleccion.Object.codigo[1] = 11
	uo_SelVariedad.Filtra(11)
	dw_1.SetTransObject(Sqlca)
	dw_2.SetTransObject(Sqlca)
	dw_3.SetTransObject(Sqlca)
	
	em_Desde.Text = '01/01/1900'
	em_Hasta.Text = String(Today(), 'dd/mm/yyyy')
	iuo_Reclamos	=	Create uo_Reclamos
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_consultareclamos
end type

type st_computador from w_para_informes`st_computador within w_info_consultareclamos
end type

type st_usuario from w_para_informes`st_usuario within w_info_consultareclamos
end type

type st_temporada from w_para_informes`st_temporada within w_info_consultareclamos
end type

type p_logo from w_para_informes`p_logo within w_info_consultareclamos
end type

type st_titulo from w_para_informes`st_titulo within w_info_consultareclamos
integer width = 1641
string text = "Consulta Reclamos"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_consultareclamos
string tag = "Imprimir Reporte"
integer x = 2021
integer y = 496
integer taborder = 100
end type

event pb_acepta::clicked;Integer	li_fila, ll_Fila_D, ll_Fila_P
Long     	ll_Numero, ll_Semana
String		ls_archivo

istr_info.titulo	= 'CONSULTA PLANILLA RECLAMOS DE ' + Upper(uo_SelEspecie.Nombre)
OpenWithParm(vinf,istr_info)

If cbx_Planilla.Checked Then
	ll_Numero = -1
Else
	ll_Numero = Long(em_Planilla.Text)
End If

If cbx_Semana.Checked Then
	ll_Semana	= -1
Else
	If IsNull(em_Semana.Text) Then
		MessageBox('Atención', 'No se ha ingresado Nro. de Semana.')
		Return -1
	Else
		ll_Semana	= Integer(em_Semana.Text)
	End If
End If

If rb_productor.Checked Then
	vinf.dw_1.DataObject = "dw_info_ctlcalplanillareclamos"	
	vinf.dw_1.Modify('DataWindow.Zoom = 95')
Else
	vinf.dw_1.DataObject = "dw_info_ctlcalplanillareclamos_expo"	
	vinf.dw_1.Modify('DataWindow.Zoom = 80')
End If

vinf.dw_1.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(gi_CodExport, uo_SelEspecie.Codigo, ll_Numero, Date(em_Desde.Text), &
									Date(em_Hasta.Text), uo_SelProductor.Codigo, uo_SelVariedad.Codigo, uo_SelRecibidor.Codigo, ll_semana)
						  
If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
		
	dw_2.Retrieve(gi_CodExport, uo_SelEspecie.Codigo, ll_Numero, Date(em_Desde.Text), &
				Date(em_Hasta.Text), uo_SelProductor.Codigo, uo_SelVariedad.Codigo, uo_SelRecibidor.Codigo, ll_semana)
				
	For ll_Fila_D = 1 To dw_2.RowCount()
		If dw_3.Retrieve(dw_2.Object.clie_codigo[ll_Fila_d], dw_2.Object.espe_codigo[ll_Fila_d], dw_2.Object.reen_numero[ll_Fila_d]) > 0 Then
			If MessageBox('Atención', 'Desea imprimir Fotos.', Exclamation!, YesNo!, 2) = 1 Then
				dw_1.Reset()
				If wf_carga_imagenes(dw_1) Then
					dw_1.Object.t_52.text = 'ANEXO RECLAMO Nº ' + String(dw_2.Object.reen_numero[ll_Fila_d])
					dw_1.Print()
				End If
			End If
		End If
		If dw_4.Retrieve(dw_2.Object.clie_codigo[ll_Fila_d], dw_2.Object.espe_codigo[ll_Fila_d], dw_2.Object.reen_numero[ll_Fila_d]) > 0 Then
		If MessageBox('Atención', 'Desea imprimir documentos anexos..', Exclamation!, YesNo!, 2) = 1 Then
			For ll_Fila_P = 1 to dw_4.RowCount()
					ls_archivo=string(dw_4.Object.repd_ruta[ll_fila_P]+ dw_4.Object.repd_archiv[ll_fila_P])
					If FileExists(ls_Archivo) Then 
						iuo_Reclamos.AbrirDocumento(ls_Archivo)
					Else
						iuo_Reclamos.RecuperaPDF(dw_4, Sqlca)
					End If
			Next
		End If
	End If
	Next
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_consultareclamos
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2011
integer y = 776
integer taborder = 110
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_info_consultareclamos
integer x = 293
integer y = 512
integer width = 411
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
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_consultareclamos
integer x = 1650
integer y = 448
integer width = 192
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
string text = "Todos"
boolean focusrectangle = false
end type

type st_6 from statictext within w_info_consultareclamos
integer x = 293
integer y = 1168
integer width = 411
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
string text = "Semana Liq"
boolean focusrectangle = false
end type

type cbx_planilla from checkbox within w_info_consultareclamos
integer x = 1701
integer y = 632
integer width = 78
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
boolean checked = true
end type

event clicked;If This.Checked Then
	em_planilla.Text = ''
	em_planilla.Enabled = False
Else
	em_planilla.Text = ''
	em_planilla.Enabled = True
	em_planilla.SetFocus()
End If
end event

type em_planilla from editmask within w_info_consultareclamos
integer x = 800
integer y = 632
integer width = 402
integer height = 92
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean enabled = false
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type uo_selespecie from uo_seleccion_especie_mod within w_info_consultareclamos
integer x = 786
integer y = 488
integer width = 878
integer taborder = 110
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie_mod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9 
		uo_SelVariedad.Filtra(-1)
		
	Case Else
		uo_SelVariedad.Filtra(This.Codigo)
		
End Choose
end event

type gb_3 from groupbox within w_info_consultareclamos
integer x = 283
integer y = 1340
integer width = 1563
integer height = 256
integer taborder = 120
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = " Fecha Reclamos "
end type

type st_2 from statictext within w_info_consultareclamos
integer x = 325
integer y = 1448
integer width = 238
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
string text = "Desde"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_consultareclamos
integer x = 1001
integer y = 1448
integer width = 210
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
string text = "Hasta"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_consultareclamos
integer x = 553
integer y = 1436
integer width = 402
integer height = 92
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
boolean dropdowncalendar = true
end type

type em_hasta from editmask within w_info_consultareclamos
integer x = 1207
integer y = 1436
integer width = 402
integer height = 92
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
boolean dropdowncalendar = true
end type

type cbx_fecha from checkbox within w_info_consultareclamos
integer x = 1705
integer y = 1440
integer width = 82
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
boolean checked = true
end type

event clicked;If This.Checked Then
	em_desde.Enabled	=	False
	em_hasta.Enabled	=	False
	em_Desde.Text		= '01/01/1900'
	em_Hasta.Text		= String(Today(), 'dd/mm/yyyy')
Else
	em_desde.Enabled	=	True
	em_hasta.Enabled	=	True
	em_Desde.Text		= '01/01/1900'
	em_Hasta.Text		= String(Today(), 'dd/mm/yyyy')	
End If
end event

type uo_selrecibidor from uo_seleccion_recibidor_mod within w_info_consultareclamos
integer x = 786
integer y = 1000
integer width = 1010
integer taborder = 120
boolean bringtotop = true
end type

on uo_selrecibidor.destroy
call uo_seleccion_recibidor_mod::destroy
end on

type uo_selproductor from uo_seleccion_productor_mod within w_info_consultareclamos
integer x = 786
integer y = 728
integer width = 1010
integer taborder = 120
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor_mod::destroy
end on

type uo_selvariedad from uo_seleccion_variedad_mod within w_info_consultareclamos
integer x = 786
integer y = 864
integer width = 1010
integer taborder = 130
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type st_5 from statictext within w_info_consultareclamos
integer x = 293
integer y = 888
integer width = 411
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
string text = "Variedad"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_consultareclamos
integer x = 293
integer y = 752
integer width = 411
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

type st_8 from statictext within w_info_consultareclamos
integer x = 293
integer y = 632
integer width = 411
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
string text = "Nro. Reclamo"
boolean focusrectangle = false
end type

type em_semana from editmask within w_info_consultareclamos
integer x = 786
integer y = 1164
integer width = 224
integer height = 96
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
string mask = "##"
end type

type st_9 from statictext within w_info_consultareclamos
integer x = 293
integer y = 1028
integer width = 411
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
string text = "Recibidor"
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_consultareclamos
integer x = 251
integer y = 440
integer width = 1641
integer height = 860
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

type cbx_semana from checkbox within w_info_consultareclamos
integer x = 1701
integer y = 1168
integer width = 78
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 553648127
boolean checked = true
end type

event clicked;If This.Checked Then
	em_semana.Text = ''
	em_Semana.Enabled = False
Else
	em_semana.Text = ''
	em_Semana.Enabled = True
	em_Semana.SetFocus()
End IF
end event

type dw_1 from uo_dw within w_info_consultareclamos
boolean visible = false
integer x = 2039
integer y = 1096
integer width = 210
integer height = 160
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_info_ctlcalreclamosimagenes"
boolean vscrollbar = false
end type

type dw_2 from uo_dw within w_info_consultareclamos
boolean visible = false
integer x = 2039
integer y = 1284
integer width = 210
integer height = 160
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_movtoreclamos"
boolean vscrollbar = false
end type

type dw_3 from uo_dw within w_info_consultareclamos
boolean visible = false
integer x = 2039
integer y = 1472
integer width = 210
integer height = 160
integer taborder = 21
boolean bringtotop = true
string dataobject = "dw_mues_ctlcalreclamosimagen"
boolean vscrollbar = false
end type

type st_11 from statictext within w_info_consultareclamos
integer x = 251
integer y = 1296
integer width = 1641
integer height = 348
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

type st_44 from statictext within w_info_consultareclamos
integer x = 251
integer y = 1644
integer width = 1641
integer height = 224
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

type rb_productor from radiobutton within w_info_consultareclamos
integer x = 425
integer y = 1712
integer width = 407
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
string text = "&Productor"
boolean checked = true
end type

type rb_exportadora from radiobutton within w_info_consultareclamos
integer x = 1221
integer y = 1712
integer width = 480
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
string text = "&Exportadora"
end type

type dw_4 from uo_dw within w_info_consultareclamos
boolean visible = false
integer x = 2039
integer y = 1664
integer width = 210
integer height = 160
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_ctlcalreclamospdf"
boolean vscrollbar = false
end type

