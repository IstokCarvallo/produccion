$PBExportHeader$w_info_resumen_descarteespecies.srw
$PBExportComments$Ventana de Informe de Resumen de palnillas de evaluación de descarte de fruta de packing
forward
global type w_info_resumen_descarteespecies from w_para_informes
end type
type em_fechadesde from editmask within w_info_resumen_descarteespecies
end type
type st_12 from statictext within w_info_resumen_descarteespecies
end type
type st_13 from statictext within w_info_resumen_descarteespecies
end type
type em_fechahasta from editmask within w_info_resumen_descarteespecies
end type
type st_zona from statictext within w_info_resumen_descarteespecies
end type
type st_tiporecep from statictext within w_info_resumen_descarteespecies
end type
type cbx_todosfecha from checkbox within w_info_resumen_descarteespecies
end type
type st_14 from statictext within w_info_resumen_descarteespecies
end type
type cbx_consfecha from checkbox within w_info_resumen_descarteespecies
end type
type st_2 from statictext within w_info_resumen_descarteespecies
end type
type uo_muestrazona from uo_seleccion_zonas_mod within w_info_resumen_descarteespecies
end type
type st_1 from statictext within w_info_resumen_descarteespecies
end type
type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_resumen_descarteespecies
end type
type st_3 from statictext within w_info_resumen_descarteespecies
end type
type st_8 from statictext within w_info_resumen_descarteespecies
end type
type gb_3 from groupbox within w_info_resumen_descarteespecies
end type
type st_44 from statictext within w_info_resumen_descarteespecies
end type
type uo_muestraproductor from uo_seleccion_productor_mod within w_info_resumen_descarteespecies
end type
type uo_muestraplanta from uo_seleccion_frigopacking_mod within w_info_resumen_descarteespecies
end type
type ddlb_tiporecep from dropdownlistbox within w_info_resumen_descarteespecies
end type
type st_6 from statictext within w_info_resumen_descarteespecies
end type
type uo_muestraespecies from uo_seleccion_especie_mod within w_info_resumen_descarteespecies
end type
type dw_1 from uo_dw within w_info_resumen_descarteespecies
end type
type st_7 from statictext within w_info_resumen_descarteespecies
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_resumen_descarteespecies
end type
type st_5 from statictext within w_info_resumen_descarteespecies
end type
end forward

global type w_info_resumen_descarteespecies from w_para_informes
string tag = "Consulta Planilla - Control de Calidad Fruta Comercial"
integer x = 14
integer y = 32
integer width = 2597
integer height = 1788
string title = "INFORME - EVALUACIÓN DE FRUTA COMERCIAL"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
em_fechadesde em_fechadesde
st_12 st_12
st_13 st_13
em_fechahasta em_fechahasta
st_zona st_zona
st_tiporecep st_tiporecep
cbx_todosfecha cbx_todosfecha
st_14 st_14
cbx_consfecha cbx_consfecha
st_2 st_2
uo_muestrazona uo_muestrazona
st_1 st_1
uo_muestravariedad uo_muestravariedad
st_3 st_3
st_8 st_8
gb_3 gb_3
st_44 st_44
uo_muestraproductor uo_muestraproductor
uo_muestraplanta uo_muestraplanta
ddlb_tiporecep ddlb_tiporecep
st_6 st_6
uo_muestraespecies uo_muestraespecies
dw_1 dw_1
st_7 st_7
uo_selcliente uo_selcliente
st_5 st_5
end type
global w_info_resumen_descarteespecies w_info_resumen_descarteespecies

type variables
str_busqueda	istr_busq
str_mant      	istr_mant
Integer	     	ii_tipo
String				is_report, is_nula


end variables

forward prototypes
public subroutine wf_visible (integer ai_especie)
public function string sacaplanta (integer ii_codplanta)
end prototypes

public subroutine wf_visible (integer ai_especie);//If ai_Especie = 26 Or ai_Especie = 27 Or ai_Especie = 78 Or ai_Especie = 81 OR ai_Especie = 36 THen
If ai_Especie = 78 Or ai_Especie = 81 OR ai_Especie = 36 THen
	ddlb_tiporecep.Visible	=	True
	st_tiporecep.Visible	=	True
	ddlb_tiporecep.SelectItem(1)
	ii_Tipo = 1
Elseif ai_Especie = 26 OR ai_Especie =  27 Then
	ddlb_tiporecep.Visible	=	True
	st_tiporecep.text        = 'Tipo Comercial'
	st_tiporecep.Visible	=	True
	ddlb_tiporecep.SelectItem(2)
	ii_Tipo = 2
	
ElseIf ai_Especie = 21 Or ai_Especie = 82 Or ai_Especie = 23 Or ai_Especie = 41 Then
	ddlb_tiporecep.Visible	=	True
	st_tiporecep.Visible	=	True
	ddlb_tiporecep.SelectItem(2)
	ii_Tipo = 2
Else
	ddlb_tiporecep.Visible	=	False
	st_tiporecep.Visible	=	False
	ddlb_tiporecep.SelectItem(1)
	ii_Tipo = 1
End If
end subroutine

public function string sacaplanta (integer ii_codplanta);String ls_planta

Select		IsNull(plde_nombre, ' ')
	Into		:ls_planta
	From		dbo.plantadesp
	Where	plde_codigo = :ii_codplanta
	Using 		Sqlca;

If Sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca,"Lectura tabla plantadesp  ")
	Return ' '
Else
	Return ls_planta
End If
end function

on w_info_resumen_descarteespecies.create
int iCurrent
call super::create
this.em_fechadesde=create em_fechadesde
this.st_12=create st_12
this.st_13=create st_13
this.em_fechahasta=create em_fechahasta
this.st_zona=create st_zona
this.st_tiporecep=create st_tiporecep
this.cbx_todosfecha=create cbx_todosfecha
this.st_14=create st_14
this.cbx_consfecha=create cbx_consfecha
this.st_2=create st_2
this.uo_muestrazona=create uo_muestrazona
this.st_1=create st_1
this.uo_muestravariedad=create uo_muestravariedad
this.st_3=create st_3
this.st_8=create st_8
this.gb_3=create gb_3
this.st_44=create st_44
this.uo_muestraproductor=create uo_muestraproductor
this.uo_muestraplanta=create uo_muestraplanta
this.ddlb_tiporecep=create ddlb_tiporecep
this.st_6=create st_6
this.uo_muestraespecies=create uo_muestraespecies
this.dw_1=create dw_1
this.st_7=create st_7
this.uo_selcliente=create uo_selcliente
this.st_5=create st_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_fechadesde
this.Control[iCurrent+2]=this.st_12
this.Control[iCurrent+3]=this.st_13
this.Control[iCurrent+4]=this.em_fechahasta
this.Control[iCurrent+5]=this.st_zona
this.Control[iCurrent+6]=this.st_tiporecep
this.Control[iCurrent+7]=this.cbx_todosfecha
this.Control[iCurrent+8]=this.st_14
this.Control[iCurrent+9]=this.cbx_consfecha
this.Control[iCurrent+10]=this.st_2
this.Control[iCurrent+11]=this.uo_muestrazona
this.Control[iCurrent+12]=this.st_1
this.Control[iCurrent+13]=this.uo_muestravariedad
this.Control[iCurrent+14]=this.st_3
this.Control[iCurrent+15]=this.st_8
this.Control[iCurrent+16]=this.gb_3
this.Control[iCurrent+17]=this.st_44
this.Control[iCurrent+18]=this.uo_muestraproductor
this.Control[iCurrent+19]=this.uo_muestraplanta
this.Control[iCurrent+20]=this.ddlb_tiporecep
this.Control[iCurrent+21]=this.st_6
this.Control[iCurrent+22]=this.uo_muestraespecies
this.Control[iCurrent+23]=this.dw_1
this.Control[iCurrent+24]=this.st_7
this.Control[iCurrent+25]=this.uo_selcliente
this.Control[iCurrent+26]=this.st_5
end on

on w_info_resumen_descarteespecies.destroy
call super::destroy
destroy(this.em_fechadesde)
destroy(this.st_12)
destroy(this.st_13)
destroy(this.em_fechahasta)
destroy(this.st_zona)
destroy(this.st_tiporecep)
destroy(this.cbx_todosfecha)
destroy(this.st_14)
destroy(this.cbx_consfecha)
destroy(this.st_2)
destroy(this.uo_muestrazona)
destroy(this.st_1)
destroy(this.uo_muestravariedad)
destroy(this.st_3)
destroy(this.st_8)
destroy(this.gb_3)
destroy(this.st_44)
destroy(this.uo_muestraproductor)
destroy(this.uo_muestraplanta)
destroy(this.ddlb_tiporecep)
destroy(this.st_6)
destroy(this.uo_muestraespecies)
destroy(this.dw_1)
destroy(this.st_7)
destroy(this.uo_selcliente)
destroy(this.st_5)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_MuestraEspecies.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_MuestraVariedad.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_MuestraZona.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(uo_MuestraProductor.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_MuestraEspecies.Seleccion(False, False)
	uo_MuestraVariedad.Seleccion(True, True)
	uo_MuestraZona.Seleccion(True, True)
	uo_MuestraProductor.Seleccion(True, True)
	uo_SelCliente.Seleccion(False, False)	

	//Especie
	uo_MuestraEspecies.dw_seleccion.Object.codigo[1] = 21
	uo_MuestraEspecies.iuo_Especie.Existe(21, False, Sqlca)
	uo_MuestraEspecies.Codigo 	= 21
	uo_MuestraEspecies.Nombre	= uo_MuestraEspecies.iuo_Especie.Nombre
	uo_MuestraVariedad.filtra(21)
	
	uo_SelCliente.dw_seleccion.Object.codigo[1] = gi_CodExport
	uo_SelCliente.Codigo = gi_CodExport
	
	em_fechadesde.Enabled	=	True
	em_fechahasta.Enabled	=	True
	em_fechadesde.text	  = String(Today())
	em_fechahasta.text	  = String(Today())
	em_fechadesde.SetFocus()
	ddlb_tiporecep.SelectItem(2)
	wf_Visible(21)
END IF
end event

event resize;call super::resize;
st_Temporada.Width		=	This.Width - p_Logo.Width - 32
st_Usuario.Width			=	This.Width - p_Logo.Width - 32
st_Computador.Width	=	This.Width - p_Logo.Width - 32
end event

type pb_excel from w_para_informes`pb_excel within w_info_resumen_descarteespecies
boolean visible = true
integer x = 2217
integer y = 340
boolean enabled = true
end type

event pb_excel::clicked;call super::clicked;Integer	li_fila, li_consfec 
Date		ld_FechaEmbaini, ld_FechaEmbafin
String		ls_Ruta, ls_Archivo,ls_planta

SetPointer(HourGlass!)

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)

//Fecha Recepción
If cbx_consfecha.Checked Then
	li_consfec  = -9
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
Else
  If cbx_todosfecha.Checked Then
	  li_consfec = -1
	  ld_FechaEmbaini =	Date(01/01/2000)
	  ld_FechaEmbafin =	Today()
	  em_fechadesde.text = String(ld_FechaEmbaini,"dd/mm/yyyy")
	  em_fechahasta.Text = String(ld_FechaEmbafin,"dd/mm/yyyy")
  Else
    ld_FechaEmbaini = Date(em_fechadesde.Text)
	 ld_FechaEmbafin = Date(em_fechahasta.Text)
  End If
End If

If uo_MuestraEspecies.Codigo = 21 Then
	If ii_Tipo = 1 Then 
 	 	dw_1.DataObject = "dw_info_consultaevaluadescartefruta_huerto"
  	Else
		//dw_1.DataObject = "dw_info_consultaevaluadescartefruta"
  		dw_1.DataObject = "dw_info_descarte_cerezas_excel"
	End If	
ElseIf uo_MuestraEspecies.Codigo = 41 Then
	dw_1.DataObject = "dw_info_frutacomercial_kiwi_excel"
ElseIf uo_MuestraEspecies.Codigo = 81 Then
	If ii_Tipo = 1 Then
	  	dw_1.DataObject = "dw_info_consultaevaluadescartepalta_huerto"
	Else
		dw_1.DataObject = "dw_info_consultaevaluadescartepalta"
	End If
ElseIf uo_MuestraEspecies.Codigo = 23 Then
	dw_1.DataObject = "dw_info_descarte_ciruela"
ElseIf uo_MuestraEspecies.Codigo = 82 Then
	dw_1.DataObject = "dw_info_descarte_granadas_excel"
Else

	IF uo_MuestraEspecies.Codigo = 26 OR uo_MuestraEspecies.Codigo = 27 OR uo_MuestraEspecies.Codigo = 36 THEN
		IF 	ii_Tipo = 1 THEN //Huerto
			IF uo_MuestraEspecies.Codigo = 26 OR uo_MuestraEspecies.Codigo = 36 THEN
				dw_1.DataObject = "dw_info_consultaevaluadescartecit_huerto_excel"
			ELSEIF uo_MuestraEspecies.Codigo = 27 THEN
				dw_1.DataObject = "dw_info_consultaevaluadescartecit_huerto_naranja_excel"
			END IF	
		ELSE //Packing
			IF uo_MuestraEspecies.Codigo = 26 OR uo_MuestraEspecies.Codigo = 36 THEN
				dw_1.DataObject = "dw_info_consultaevaluadescartecit_excel"
			ELSEIF uo_MuestraEspecies.Codigo = 27 THEN
				dw_1.DataObject = "dw_info_consultaevaluadescartecit_naranja_excel"
			END IF
		END IF
	END IF
End If

ls_planta = sacaplanta(gi_codplanta)
ls_Archivo = '\Descarte_' + uo_MuestraEspecies.Nombre + '_' +trim(ls_planta)+'.xls'

dw_1.SetTransObject(sqlca)

If uo_MuestraEspecies.Codigo = 23 Or uo_MuestraEspecies.Codigo = 82 Then
	li_fila = dw_1.Retrieve(uo_SelCliente.Codigo,uo_MuestraEspecies.Codigo, -1, ii_Tipo, uo_MuestraVariedad.Codigo,&
						uo_MuestraZona.Codigo, uo_MuestraPlanta.Codigo, uo_MuestraProductor.Codigo, ld_FechaEmbaini, ld_FechaEmbafin)
ElseIf uo_MuestraEspecies.Codigo = 21 Then
	li_fila = dw_1.Retrieve(uo_SelCliente.Codigo,uo_MuestraEspecies.Codigo,uo_MuestraVariedad.Codigo,uo_MuestraZona.Codigo,&
					uo_MuestraPlanta.Codigo, uo_MuestraProductor.Codigo, ld_FechaEmbaini, ld_FechaEmbafin, li_consfec, ii_Tipo, -1)
Else
	li_fila = dw_1.Retrieve(uo_SelCliente.Codigo,uo_MuestraEspecies.Codigo,uo_MuestraVariedad.Codigo,uo_MuestraZona.Codigo,&
					uo_MuestraPlanta.Codigo, uo_MuestraProductor.Codigo, ld_FechaEmbaini, ld_FechaEmbafin, li_consfec, ii_Tipo)
End If

If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
Else
	If dw_1.SaveAs(ls_Ruta + ls_Archivo, Excel8!, True) = 1 Then
		MessageBox('Atencion', 'Se genero archivo ' + ls_Archivo)
	Else
		MessageBox('Atencion', 'No se pudo generar archivo ' + ls_Archivo)		
	End If	
End If

SetPointer(Arrow!)
end event

type st_computador from w_para_informes`st_computador within w_info_resumen_descarteespecies
end type

type st_usuario from w_para_informes`st_usuario within w_info_resumen_descarteespecies
end type

type st_temporada from w_para_informes`st_temporada within w_info_resumen_descarteespecies
end type

type p_logo from w_para_informes`p_logo within w_info_resumen_descarteespecies
end type

type st_titulo from w_para_informes`st_titulo within w_info_resumen_descarteespecies
integer width = 1842
integer height = 88
string text = "Informe Fruta Comercial"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resumen_descarteespecies
string tag = "Imprimir Reporte"
integer x = 2208
integer y = 644
integer taborder = 120
integer weight = 400
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;Integer	li_fila, li_consfec 
Date		ld_FechaEmbaini, ld_FechaEmbafin

//Fecha Recepción
If cbx_consfecha.Checked Then
	li_consfec  = -9
	ld_FechaEmbaini =	Date(01/01/2000)
	ld_FechaEmbafin =	Today()
Else
  If cbx_todosfecha.Checked Then
	  li_consfec = -1
	  ld_FechaEmbaini =	Date(01/01/2000)
	  ld_FechaEmbafin =	Today()
	  em_fechadesde.text = String(ld_FechaEmbaini,"dd/mm/yyyy")
	  em_fechahasta.Text = String(ld_FechaEmbafin,"dd/mm/yyyy")
  Else
    ld_FechaEmbaini = Date(em_fechadesde.Text)
	 ld_FechaEmbafin = Date(em_fechahasta.Text)
  End If
End If

istr_info.titulo	= 'INFORME - EVALUACIÓN DE FRUTA DE DESCARTE DE PACKING DE'

OpenWithParm(vinf,istr_info)

If uo_MuestraEspecies.Codigo = 21 Then
	If ii_Tipo = 1 Then 
 	 	vinf.dw_1.DataObject = "dw_info_consultaevaluadescartefruta_huerto"
  		vinf.dw_1.Object.DataWindow.Zoom = 75
	Else
		vinf.dw_1.DataObject = "dw_info_consultaevaluadescartefruta"
  		vinf.dw_1.Object.DataWindow.Zoom = 90
	End If
ElseIf uo_MuestraEspecies.Codigo = 41 Then
 	vinf.dw_1.DataObject = "dw_info_consultaevaluadescartekiwis"
 	vinf.dw_1.Object.DataWindow.Zoom = 85
ElseIf uo_MuestraEspecies.Codigo = 81 Then
	If ii_Tipo = 1 Then
	  	vinf.dw_1.DataObject = "dw_info_consultaevaluadescartepalta_huerto"
  		vinf.dw_1.Object.DataWindow.Zoom = 92
	Else
		vinf.dw_1.DataObject = "dw_info_consultaevaluadescartepalta"
  		vinf.dw_1.Object.DataWindow.Zoom = 78
	End If
ElseIf uo_MuestraEspecies.Codigo = 23 Then
	vinf.dw_1.DataObject = "dw_info_descarte_ciruela"
	vinf.dw_1.Object.DataWindow.Zoom = 78
ElseIf uo_MuestraEspecies.Codigo = 82 Then
	vinf.dw_1.DataObject = "dw_info_descarte_granadas"
	vinf.dw_1.Object.DataWindow.Zoom = 80
Else
	If ii_Tipo = 1 Then
		vinf.dw_1.DataObject = "dw_info_consultaevaluadescartecit_huerto"
		vinf.dw_1.Object.DataWindow.Zoom = 74
	Else
		vinf.dw_1.DataObject = "dw_info_consultaevaluadescartecit"
   		vinf.dw_1.Object.DataWindow.Zoom =67
	End If	
End If

vinf.dw_1.SetTransObject(sqlca)
If uo_MuestraEspecies.Codigo = 23 Or uo_MuestraEspecies.Codigo = 82 Then
	li_fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo,uo_MuestraEspecies.Codigo, -1, ii_Tipo, uo_MuestraVariedad.Codigo,&
						uo_MuestraZona.Codigo, uo_MuestraPlanta.Codigo, uo_MuestraProductor.Codigo, ld_FechaEmbaini, ld_FechaEmbafin)
ElseIf uo_MuestraEspecies.Codigo = 21 Then
	li_fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo,uo_MuestraEspecies.Codigo,uo_MuestraVariedad.Codigo,uo_MuestraZona.Codigo,&
					uo_MuestraPlanta.Codigo, uo_MuestraProductor.Codigo, ld_FechaEmbaini, ld_FechaEmbafin, li_consfec, ii_Tipo, -1)
Else
	li_fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo,uo_MuestraEspecies.Codigo,uo_MuestraVariedad.Codigo,uo_MuestraZona.Codigo,&
					uo_MuestraPlanta.Codigo, uo_MuestraProductor.Codigo, ld_FechaEmbaini, ld_FechaEmbafin, li_consfec, ii_Tipo)
End If

If li_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf li_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy("desde.text = '" + String(ld_FechaEmbaini,"dd/mm/yyyy") + "'")
	vinf.dw_1.ModIfy("hasta.text = '" + String(ld_FechaEmbafin,"dd/mm/yyyy") + "'")	
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If
end event

type pb_salir from w_para_informes`pb_salir within w_info_resumen_descarteespecies
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2213
integer taborder = 130
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type em_fechadesde from editmask within w_info_resumen_descarteespecies
integer x = 613
integer y = 1400
integer width = 411
integer height = 92
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_12 from statictext within w_info_resumen_descarteespecies
integer x = 389
integer y = 1420
integer width = 215
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
boolean focusrectangle = false
end type

type st_13 from statictext within w_info_resumen_descarteespecies
integer x = 1029
integer y = 1412
integer width = 178
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
boolean focusrectangle = false
end type

type em_fechahasta from editmask within w_info_resumen_descarteespecies
integer x = 1202
integer y = 1400
integer width = 411
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_zona from statictext within w_info_resumen_descarteespecies
integer x = 279
integer y = 872
integer width = 466
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

type st_tiporecep from statictext within w_info_resumen_descarteespecies
integer x = 279
integer y = 1208
integer width = 466
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
string text = "Tipo Recep."
boolean focusrectangle = false
end type

type cbx_todosfecha from checkbox within w_info_resumen_descarteespecies
integer x = 1669
integer y = 1412
integer width = 91
integer height = 64
integer taborder = 90
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

event clicked;IF This.Checked THEN
	em_fechadesde.Enabled	=	FALSE
	em_fechahasta.Enabled	=	FALSE
   istr_Mant.Argumento[12]	=	'01/01/1900'
	istr_Mant.Argumento[13]	=	String(Today())
	//cbx_consfecha.enabled   =  TRUE
	cbx_consfecha.checked   =  FALSE
ELSE
	em_fechadesde.Enabled	=	TRUE
	em_fechahasta.Enabled	=	TRUE
	em_fechadesde.SetFocus()
	//cbx_consfecha.enabled   =  FALSE
END IF
RETURN 0



end event

type st_14 from statictext within w_info_resumen_descarteespecies
integer x = 279
integer y = 660
integer width = 466
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

type cbx_consfecha from checkbox within w_info_resumen_descarteespecies
integer x = 1870
integer y = 1412
integer width = 91
integer height = 64
integer taborder = 100
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

event clicked;IF This.Checked THEN
	em_fechadesde.Enabled	=	FALSE
	em_fechahasta.Enabled	=	FALSE
   istr_Mant.Argumento[12]	=	'01/01/1900'
	istr_Mant.Argumento[13]	=	String(Today())
	//cbx_consfecha.enabled   =  TRUE
	cbx_todosfecha.Checked = False
ELSE
	em_fechadesde.Enabled	=	TRUE
	em_fechahasta.Enabled	=	TRUE
	em_fechadesde.SetFocus()
	//cbx_consfecha.enabled   =  FALSE
END IF
RETURN 0
end event

type st_2 from statictext within w_info_resumen_descarteespecies
integer x = 1586
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

type uo_muestrazona from uo_seleccion_zonas_mod within w_info_resumen_descarteespecies
integer x = 750
integer y = 856
integer height = 120
integer taborder = 30
boolean bringtotop = true
end type

on uo_muestrazona.destroy
call uo_seleccion_zonas_mod::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

/*comentado por filtro de packing*/
//CHOOSE CASE This.Codigo
//	CASE -1, -9
//		uo_muestraplanta.Todos(True)
//		uo_muestraplanta.cbx_Todos.Enabled	=	False
//		
//	CASE ELSE
//		
//		uo_muestraplanta.Filtra(This.Codigo)
//		uo_muestraplanta.cbx_Todos.Enabled	=	True
//END CHOOSE
end event

type st_1 from statictext within w_info_resumen_descarteespecies
integer x = 279
integer y = 988
integer width = 466
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
string text = "Planta"
boolean focusrectangle = false
end type

type uo_muestravariedad from uo_seleccion_variedad_mod within w_info_resumen_descarteespecies
integer x = 750
integer y = 752
integer width = 1253
integer height = 120
integer taborder = 20
boolean bringtotop = true
end type

on uo_muestravariedad.destroy
call uo_seleccion_variedad_mod::destroy
end on

type st_3 from statictext within w_info_resumen_descarteespecies
integer x = 279
integer y = 776
integer width = 466
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

type st_8 from statictext within w_info_resumen_descarteespecies
integer x = 1838
integer y = 464
integer width = 201
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
string text = "Cons."
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_resumen_descarteespecies
integer x = 288
integer y = 1304
integer width = 1769
integer height = 252
integer taborder = 110
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = " Fecha Proceso "
end type

type st_44 from statictext within w_info_resumen_descarteespecies
integer x = 251
integer y = 440
integer width = 1842
integer height = 1168
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

type uo_muestraproductor from uo_seleccion_productor_mod within w_info_resumen_descarteespecies
event destroy ( )
integer x = 750
integer y = 1076
integer width = 1253
integer height = 120
integer taborder = 50
boolean bringtotop = true
end type

on uo_muestraproductor.destroy
call uo_seleccion_productor_mod::destroy
end on

type uo_muestraplanta from uo_seleccion_frigopacking_mod within w_info_resumen_descarteespecies
integer x = 750
integer y = 968
integer height = 120
integer taborder = 40
boolean bringtotop = true
end type

on uo_muestraplanta.destroy
call uo_seleccion_frigopacking_mod::destroy
end on

type ddlb_tiporecep from dropdownlistbox within w_info_resumen_descarteespecies
integer x = 754
integer y = 1208
integer width = 873
integer height = 400
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 134217742
boolean sorted = false
string item[] = {"Huerto","Packing"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_Tipo = Index	
end event

type st_6 from statictext within w_info_resumen_descarteespecies
integer x = 279
integer y = 1092
integer width = 466
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

type uo_muestraespecies from uo_seleccion_especie_mod within w_info_resumen_descarteespecies
integer x = 750
integer y = 640
integer height = 120
integer taborder = 10
boolean bringtotop = true
end type

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
				
	Case Else
		uo_muestravariedad.Filtra(This.Codigo)
		uo_muestravariedad.cbx_Todos.Enabled	=	True
		wf_Visible(This.Codigo)
		
End Choose
end event

on uo_muestraespecies.destroy
call uo_seleccion_especie_mod::destroy
end on

type dw_1 from uo_dw within w_info_resumen_descarteespecies
boolean visible = false
integer x = 2226
integer y = 1200
integer taborder = 11
boolean bringtotop = true
boolean vscrollbar = false
end type

type st_7 from statictext within w_info_resumen_descarteespecies
integer x = 279
integer y = 544
integer width = 466
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
string text = "Cliente"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_resumen_descarteespecies
integer x = 750
integer y = 544
integer height = 84
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_5 from statictext within w_info_resumen_descarteespecies
boolean visible = false
integer x = 251
integer y = 284
integer width = 1856
integer height = 88
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Control de Calidad Fruta Comercial"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

