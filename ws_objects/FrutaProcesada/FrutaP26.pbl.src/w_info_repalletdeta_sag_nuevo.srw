$PBExportHeader$w_info_repalletdeta_sag_nuevo.srw
$PBExportComments$Informe de Revisión Repalletizajes.
forward
global type w_info_repalletdeta_sag_nuevo from w_para_informes
end type
type st_4 from statictext within w_info_repalletdeta_sag_nuevo
end type
type st_1 from statictext within w_info_repalletdeta_sag_nuevo
end type
type st_5 from statictext within w_info_repalletdeta_sag_nuevo
end type
type st_2 from statictext within w_info_repalletdeta_sag_nuevo
end type
type em_numero from editmask within w_info_repalletdeta_sag_nuevo
end type
type st_6 from statictext within w_info_repalletdeta_sag_nuevo
end type
type cb_buscarepa from commandbutton within w_info_repalletdeta_sag_nuevo
end type
type st_7 from statictext within w_info_repalletdeta_sag_nuevo
end type
type st_8 from statictext within w_info_repalletdeta_sag_nuevo
end type
type em_repasag from editmask within w_info_repalletdeta_sag_nuevo
end type
type sle_fecha from singlelineedit within w_info_repalletdeta_sag_nuevo
end type
type st_3 from statictext within w_info_repalletdeta_sag_nuevo
end type
type st_9 from statictext within w_info_repalletdeta_sag_nuevo
end type
type st_10 from statictext within w_info_repalletdeta_sag_nuevo
end type
type em_pais from editmask within w_info_repalletdeta_sag_nuevo
end type
type st_11 from statictext within w_info_repalletdeta_sag_nuevo
end type
type st_12 from statictext within w_info_repalletdeta_sag_nuevo
end type
type em_observaciones from editmask within w_info_repalletdeta_sag_nuevo
end type
type st_13 from statictext within w_info_repalletdeta_sag_nuevo
end type
type cbx_archivo from checkbox within w_info_repalletdeta_sag_nuevo
end type
type ddlb_accion from dropdownlistbox within w_info_repalletdeta_sag_nuevo
end type
type st_14 from statictext within w_info_repalletdeta_sag_nuevo
end type
type st_15 from statictext within w_info_repalletdeta_sag_nuevo
end type
type cbx_1 from checkbox within w_info_repalletdeta_sag_nuevo
end type
type cbx_2 from checkbox within w_info_repalletdeta_sag_nuevo
end type
type st_19 from statictext within w_info_repalletdeta_sag_nuevo
end type
type em_contraparte from singlelineedit within w_info_repalletdeta_sag_nuevo
end type
type st_16 from statictext within w_info_repalletdeta_sag_nuevo
end type
type cbx_pda from radiobutton within w_info_repalletdeta_sag_nuevo
end type
type cbx_csp from radiobutton within w_info_repalletdeta_sag_nuevo
end type
type cbx_nuevoformato from radiobutton within w_info_repalletdeta_sag_nuevo
end type
type cbx_soloinspec from checkbox within w_info_repalletdeta_sag_nuevo
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_repalletdeta_sag_nuevo
end type
type uo_selplanta from uo_seleccion_plantas within w_info_repalletdeta_sag_nuevo
end type
end forward

global type w_info_repalletdeta_sag_nuevo from w_para_informes
integer x = 14
integer y = 32
integer width = 2674
integer height = 2144
string title = "Informe de Repaletización S.A.G."
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_5 st_5
st_2 st_2
em_numero em_numero
st_6 st_6
cb_buscarepa cb_buscarepa
st_7 st_7
st_8 st_8
em_repasag em_repasag
sle_fecha sle_fecha
st_3 st_3
st_9 st_9
st_10 st_10
em_pais em_pais
st_11 st_11
st_12 st_12
em_observaciones em_observaciones
st_13 st_13
cbx_archivo cbx_archivo
ddlb_accion ddlb_accion
st_14 st_14
st_15 st_15
cbx_1 cbx_1
cbx_2 cbx_2
st_19 st_19
em_contraparte em_contraparte
st_16 st_16
cbx_pda cbx_pda
cbx_csp cbx_csp
cbx_nuevoformato cbx_nuevoformato
cbx_soloinspec cbx_soloinspec
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
end type
global w_info_repalletdeta_sag_nuevo w_info_repalletdeta_sag_nuevo

type variables
Date			id_FechaAltu
integer		ii_accion

str_busqueda istr_busq
str_mant istr_mant
end variables

forward prototypes
public function boolean noexistefolioaltura (long al_numero)
public function string contraparte (integer ai_planta)
end prototypes

public function boolean noexistefolioaltura (long al_numero);Integer	li_CantTarjasCambio
Long		li_cantidad 
	
SELECT	altu_fecmov
	INTO	:id_FechaAltu
	FROM	dbo.ALPALLETENCAB
	WHERE	clie_codigo	=	:uo_SelCliente.Codigo
	AND	plde_codigo		=	:uo_SelPlanta.Codigo
	AND	altu_numero	=	:al_Numero;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla AlPalletencab")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Cambio Altura no ha sido Ingresado.~r~r" + &
					"Ingrese o seleccione otro Número.", Exclamation!, Ok!)
	RETURN True
ELSE
	SELECT	Count(paen_numero)
		INTO	:li_CantTarjasCambio
		FROM	dbo.ALPALLETFRUTA
		WHERE	clie_codigo	=	:uo_SelCliente.Codigo
		AND	plde_codigo		=	:uo_SelPlanta.Codigo
		AND	altu_numero	=	:al_Numero;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Repalletizajes")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Error", "Número de Cambio de Altura no tiene Detalle.~r~r" + &
						"Consulte con Encargado.", Exclamation!, Ok!)
		RETURN True
	END IF
	RETURN False
END IF
end function

public function string contraparte (integer ai_planta);String	ls_nombre,ls_paterm,ls_mater

SELECT RTrim(tecn_nombre),RTrim(tecn_apepat),RTrim(tecn_apemat)
INTO :ls_nombre,:ls_paterm,:ls_mater
FROM dbo.cargostecnicos
WHERE plde_codigo = :ai_Planta
AND	tecn_codigo = 1
AND	tecn_tprofe = 1;

return ls_nombre+' '+ls_paterm+' '+ls_mater




end function

on w_info_repalletdeta_sag_nuevo.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_5=create st_5
this.st_2=create st_2
this.em_numero=create em_numero
this.st_6=create st_6
this.cb_buscarepa=create cb_buscarepa
this.st_7=create st_7
this.st_8=create st_8
this.em_repasag=create em_repasag
this.sle_fecha=create sle_fecha
this.st_3=create st_3
this.st_9=create st_9
this.st_10=create st_10
this.em_pais=create em_pais
this.st_11=create st_11
this.st_12=create st_12
this.em_observaciones=create em_observaciones
this.st_13=create st_13
this.cbx_archivo=create cbx_archivo
this.ddlb_accion=create ddlb_accion
this.st_14=create st_14
this.st_15=create st_15
this.cbx_1=create cbx_1
this.cbx_2=create cbx_2
this.st_19=create st_19
this.em_contraparte=create em_contraparte
this.st_16=create st_16
this.cbx_pda=create cbx_pda
this.cbx_csp=create cbx_csp
this.cbx_nuevoformato=create cbx_nuevoformato
this.cbx_soloinspec=create cbx_soloinspec
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_5
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_numero
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.cb_buscarepa
this.Control[iCurrent+8]=this.st_7
this.Control[iCurrent+9]=this.st_8
this.Control[iCurrent+10]=this.em_repasag
this.Control[iCurrent+11]=this.sle_fecha
this.Control[iCurrent+12]=this.st_3
this.Control[iCurrent+13]=this.st_9
this.Control[iCurrent+14]=this.st_10
this.Control[iCurrent+15]=this.em_pais
this.Control[iCurrent+16]=this.st_11
this.Control[iCurrent+17]=this.st_12
this.Control[iCurrent+18]=this.em_observaciones
this.Control[iCurrent+19]=this.st_13
this.Control[iCurrent+20]=this.cbx_archivo
this.Control[iCurrent+21]=this.ddlb_accion
this.Control[iCurrent+22]=this.st_14
this.Control[iCurrent+23]=this.st_15
this.Control[iCurrent+24]=this.cbx_1
this.Control[iCurrent+25]=this.cbx_2
this.Control[iCurrent+26]=this.st_19
this.Control[iCurrent+27]=this.em_contraparte
this.Control[iCurrent+28]=this.st_16
this.Control[iCurrent+29]=this.cbx_pda
this.Control[iCurrent+30]=this.cbx_csp
this.Control[iCurrent+31]=this.cbx_nuevoformato
this.Control[iCurrent+32]=this.cbx_soloinspec
this.Control[iCurrent+33]=this.uo_selcliente
this.Control[iCurrent+34]=this.uo_selplanta
end on

on w_info_repalletdeta_sag_nuevo.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.em_numero)
destroy(this.st_6)
destroy(this.cb_buscarepa)
destroy(this.st_7)
destroy(this.st_8)
destroy(this.em_repasag)
destroy(this.sle_fecha)
destroy(this.st_3)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.em_pais)
destroy(this.st_11)
destroy(this.st_12)
destroy(this.em_observaciones)
destroy(this.st_13)
destroy(this.cbx_archivo)
destroy(this.ddlb_accion)
destroy(this.st_14)
destroy(this.st_15)
destroy(this.cbx_1)
destroy(this.cbx_2)
destroy(this.st_19)
destroy(this.em_contraparte)
destroy(this.st_16)
destroy(this.cbx_pda)
destroy(this.cbx_csp)
destroy(this.cbx_nuevoformato)
destroy(this.cbx_soloinspec)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
end on

event open;call super::open;Boolean	lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else	
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	
	uo_SelPlanta.Filtra(1)
	
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gi_CodPlanta)

	istr_mant.argumento[2]	=	""
	istr_mant.argumento[4]	=	'0'
	istr_mant.argumento[6]	=	""	// Altura
	
	ddlb_accion.SelectItem(1)
	
	ii_accion = 1
	
	em_contraparte.Text = ContraParte(gi_codplanta)
	
	ids_archivo					=	CREATE	DataStore
	ids_archivo.DataObject	=	'dw_info_repalletdeta_nusag_excel'
	ids_archivo.SetTransObject(sqlca)
	
	ids_archivo2				=	CREATE	DataStore
	ids_archivo2.DataObject	=	'dw_info_repalletdeta_nusag2_excel'
	ids_archivo2.SetTransObject(sqlca)
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_repalletdeta_sag_nuevo
end type

type st_computador from w_para_informes`st_computador within w_info_repalletdeta_sag_nuevo
end type

type st_usuario from w_para_informes`st_usuario within w_info_repalletdeta_sag_nuevo
end type

type st_temporada from w_para_informes`st_temporada within w_info_repalletdeta_sag_nuevo
end type

type p_logo from w_para_informes`p_logo within w_info_repalletdeta_sag_nuevo
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_repalletdeta_sag_nuevo
integer width = 1902
string text = "Informe de Repalletizado S.A.G."
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_repalletdeta_sag_nuevo
string tag = "Imprimir Reporte"
integer x = 2309
integer y = 1444
integer taborder = 60
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila,li_cbx_planta,li_especie,li_numero, li_inspeccion
Long		ll_nrorep
String		ls_region, ls_especie, ls_Archivo, ls_Ruta

ll_nrorep			=	Long(em_numero.Text)

If Integer(uo_SelPlanta.Region) = 1 Then ls_region = 'I Región'
If Integer(uo_SelPlanta.Region) = 2 Then ls_region = 'II Región'
If Integer(uo_SelPlanta.Region) = 3 Then ls_region = 'III Región'
If Integer(uo_SelPlanta.Region) = 4 Then ls_region = 'IV Región'
If Integer(uo_SelPlanta.Region) = 5 Then ls_region = 'V Región'
If Integer(uo_SelPlanta.Region) = 6 Then ls_region = 'VI Región'
If Integer(uo_SelPlanta.Region) = 7 Then ls_region = 'VII Región'
If Integer(uo_SelPlanta.Region) = 8 Then ls_region = 'VIII Región'
If Integer(uo_SelPlanta.Region) = 9 Then ls_region = 'IX Región'
If Integer(uo_SelPlanta.Region) = 10 Then ls_region = 'X Región'
If Integer(uo_SelPlanta.Region) = 11 Then ls_region = 'XI Región'
If Integer(uo_SelPlanta.Region) = 12 Then ls_region = 'XII Región'
If Integer(uo_SelPlanta.Region) = 13 Then ls_region = 'Región Metropolitana'

SELECT es.espe_nombre
INTO   :ls_especie
FROM dbo.especies as es, dbo.repalletdeta as rd, dbo.palletencab as pe
WHERE rd.clie_codigo=pe.clie_codigo
AND   rd.plde_codigo=pe.plde_codigo
AND	rd.paen_numero=pe.paen_numero
AND   pe.espe_codigo=es.espe_codigo
AND   rd.clie_codigo=:uo_SelCliente.Codigo
AND   rd.plde_codigo=:uo_SelPlanta.Codigo
AND   rd.repe_numero=:ll_nrorep;

If Long(istr_mant.argumento[4]) = 0 Then
	MessageBox("Atención", "Repalletizado S.A.G. debe ser Superior a 0.~r~rIngrese otro Número.", Exclamation!, Ok!)
	em_repasag.SetFocus()
ElseIf cbx_archivo.Checked Then
	ls_Archivo	=	"\Repalletizados-"+String(Long(istr_mant.Argumento[4]))+"-Original.xls"

	ids_archivo.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo,Long(istr_mant.Argumento[2]), 1,0,-1,Long(istr_mant.Argumento[6]))
	
	RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
	ids_archivo.SaveAs(ls_Ruta + ls_Archivo,Excel8!, True)	
	ls_Archivo	=	"\Repalletizados-"+String(Long(istr_mant.Argumento[4]))+"-Repaletizado.xls"

	ids_archivo2.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, Long(istr_mant.Argumento[2]), 2,0,1, Long(istr_mant.Argumento[6]))
	
	RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
	ids_archivo2.SaveAs(ls_Ruta + ls_Archivo,Excel8!, True)			
	MessageBox("Atención","Archivo Formato Excel, Generado.")	
Else
	istr_info.titulo	= 'INFORME DE REPALETIZADO S.A.G.'

	OpenWithParm(vinf, istr_info)
	
	If cbx_csp.Checked Then
		If cbx_pda.Checked Then
			vinf.dw_1.DataObject = "dw_info_repalletdeta_sag_unisag_pda_nuevo"
		Else
			vinf.dw_1.DataObject = "dw_info_repalletdeta_sag_unisag_format_nuevo"
		End If	
	Else
		If cbx_pda.Checked Then
			vinf.dw_1.DataObject = "dw_info_repalletdeta_sag_unisag_nuevo_formato_pda"
		Else
			vinf.dw_1.DataObject = "dw_info_repalletdeta_sag_unisag_nuevo_formato"
		End If	
	End If	
	
	vinf.dw_1.SetTransObject(sqlca)
	
	If cbx_soloinspec.Checked Then
		li_inspeccion = 1
	Else
		li_inspeccion = -1
	End If	
		
	fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, Long(istr_mant.Argumento[2]), Long(istr_mant.Argumento[6]),ii_accion,li_inspeccion)
	If fila = -1 Then
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
						"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ElseIf fila = 0 Then
		MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
	Else
		F_Membrete(vinf.dw_1)
		If cbx_1.Checked Then vinf.dw_1.ModIfy("t_supervisado.text = '" + 'X' + "'")
		If cbx_2.Checked Then vinf.dw_1.ModIfy("t_nosupervisado.text = '" + 'X' + "'")
		
		vinf.dw_1.ModIfy("frigorIfico.text = '" + uo_SelPlanta.Nombre + "'")
		vinf.dw_1.ModIfy("fecha.text = '" + String(Date(istr_mant.Argumento[10])) + "'")
		vinf.dw_1.ModIfy("repalesag.text = '" + istr_mant.Argumento[4] + "'")	
		vinf.dw_1.ModIfy("region.text = '" + ls_Region + "'")		
		vinf.dw_1.ModIfy("especie.text = '" + ls_especie + "'")	
		vinf.dw_1.ModIfy("codsag.text = '" + String(uo_SelPlanta.CodigoSAG) + "'")	
		vinf.dw_1.ModIfy("t_contraparte.text = '" + em_contraparte.Text + "'")	
		vinf.dw_1.ModIfy("t_pais.text = '" + em_pais.Text + "'")
		vinf.dw_1.ModIfy("t_observa.text = '" + em_observaciones.Text + "'")			
		If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	End If
End If

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_repalletdeta_sag_nuevo
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2304
integer y = 1752
integer taborder = 70
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_repalletdeta_sag_nuevo
integer x = 247
integer y = 440
integer width = 1902
integer height = 256
boolean bringtotop = true
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

type st_1 from statictext within w_info_repalletdeta_sag_nuevo
integer x = 302
integer y = 588
integer width = 462
integer height = 76
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_repalletdeta_sag_nuevo
integer x = 247
integer y = 700
integer width = 1902
integer height = 144
boolean bringtotop = true
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

type st_2 from statictext within w_info_repalletdeta_sag_nuevo
integer x = 311
integer y = 732
integer width = 517
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
string text = "N° Repaletizado"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_numero from editmask within w_info_repalletdeta_sag_nuevo
integer x = 951
integer y = 720
integer width = 393
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Long		ll_numero, ll_nrosag
Date		ld_fzarpe
ll_numero		=	Long(This.Text)
	
SELECT	repe_fecrep
  INTO	:ld_fzarpe
  FROM	dbo.REPALLETENCA
 WHERE	clie_codigo	=	:uo_SelCliente.Codigo
	AND	plde_codigo =	:uo_SelPlanta.Codigo
	AND   repe_numero =  :ll_numero;
				
IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla REPALLETENCA")
		em_numero.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Repalletizado Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		em_numero.SetFocus()
ELSE
	SELECT	max(repe_nrosag)
	  INTO	:ll_nrosag
	  FROM	dbo.REPALLETENCA
	 WHERE	clie_codigo	=	:uo_SelCliente.Codigo
		AND	plde_codigo =	:uo_SelPlanta.Codigo
		AND   repe_numero =  :ll_numero;
		
	istr_mant.argumento[10]	=	String(ld_fzarpe)
	istr_mant.argumento[2]	=	This.Text		
	sle_fecha.text				= 	String(ld_fzarpe, 'dd/mm/yyyy')
	em_repasag.Text			=	String(ll_nrosag)
	istr_mant.argumento[4]	=  String(ll_nrosag)
END IF

end event

type st_6 from statictext within w_info_repalletdeta_sag_nuevo
integer x = 302
integer y = 484
integer width = 233
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
string text = "Cliente"
boolean focusrectangle = false
end type

type cb_buscarepa from commandbutton within w_info_repalletdeta_sag_nuevo
integer x = 1367
integer y = 724
integer width = 91
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;Long		ll_nrosag, ll_numero

istr_busq.argum[1]	=	String(uo_SelCliente.Codigo)
istr_busq.argum[2]	=	String(uo_SelPlanta.Codigo)
istr_busq.argum[3]	=	'1'

OpenWithParm(w_busc_repalletenca_sag, istr_busq)

istr_busq	       = Message.PowerObjectParm

IF istr_busq.argum[5] <> "" THEN
	
	ll_numero = Long(istr_busq.argum[5])
	
	SELECT	max(repe_nrosag)
	  INTO	:ll_nrosag
	  FROM	dbo.REPALLETENCA
	 WHERE	clie_codigo	=	:uo_SelCliente.Codigo
		AND	plde_codigo =	:uo_SelPlanta.Codigo
		AND   repe_numero =  :ll_numero;
		
	em_numero.Text				= 	istr_busq.argum[5]
	istr_mant.argumento[2]		= 	istr_busq.argum[5]
	istr_mant.argumento[10]	=	istr_busq.argum[10]
	sle_fecha.Text					=	istr_busq.argum[10]
	em_repasag.Text				=	String(ll_nrosag)
	istr_mant.argumento[4]		=  String(ll_nrosag)
ELSE
	em_numero.SetFocus()
END IF
end event

type st_7 from statictext within w_info_repalletdeta_sag_nuevo
integer x = 247
integer y = 844
integer width = 1902
integer height = 124
boolean bringtotop = true
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

type st_8 from statictext within w_info_repalletdeta_sag_nuevo
integer x = 311
integer y = 868
integer width = 549
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
string text = "Correlativo S.A.G."
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_repasag from editmask within w_info_repalletdeta_sag_nuevo
integer x = 951
integer y = 856
integer width = 393
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
end type

event modified;istr_mant.argumento[4]	=	This.Text

IF Long(This.Text) = 0 THEN
		MessageBox("Atención", "Repalletizado S.A.G. debe ser Superior a 0.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		em_repasag.SetFocus()
	END IF
end event

type sle_fecha from singlelineedit within w_info_repalletdeta_sag_nuevo
integer x = 1481
integer y = 724
integer width = 393
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
borderstyle borderstyle = stylelowered!
end type

type st_3 from statictext within w_info_repalletdeta_sag_nuevo
integer x = 247
integer y = 968
integer width = 1902
integer height = 180
boolean bringtotop = true
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

type st_9 from statictext within w_info_repalletdeta_sag_nuevo
integer x = 247
integer y = 1148
integer width = 1902
integer height = 128
boolean bringtotop = true
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

type st_10 from statictext within w_info_repalletdeta_sag_nuevo
integer x = 311
integer y = 1172
integer width = 640
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
string text = "País S.A.G."
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_pais from editmask within w_info_repalletdeta_sag_nuevo
integer x = 901
integer y = 1164
integer width = 1134
integer height = 92
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type st_11 from statictext within w_info_repalletdeta_sag_nuevo
integer x = 247
integer y = 1276
integer width = 1902
integer height = 312
boolean bringtotop = true
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

type st_12 from statictext within w_info_repalletdeta_sag_nuevo
integer x = 311
integer y = 1408
integer width = 640
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
string text = "Observaciones S.A.G."
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_observaciones from editmask within w_info_repalletdeta_sag_nuevo
integer x = 315
integer y = 1472
integer width = 1778
integer height = 92
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type st_13 from statictext within w_info_repalletdeta_sag_nuevo
integer x = 247
integer y = 1740
integer width = 1902
integer height = 136
boolean bringtotop = true
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

type cbx_archivo from checkbox within w_info_repalletdeta_sag_nuevo
integer x = 379
integer y = 1776
integer width = 320
integer height = 64
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Archivo"
end type

type ddlb_accion from dropdownlistbox within w_info_repalletdeta_sag_nuevo
integer x = 1266
integer y = 1752
integer width = 805
integer height = 532
integer taborder = 190
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 33543637
boolean sorted = false
boolean hscrollbar = true
boolean vscrollbar = true
string item[] = {"Inspección","Tratamiento","Repaletizaje","Interplanta","Fumigación","Otro",""}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_accion = index

IF index = 5 THEN
	ii_accion = 6
ELSEIF index = 6 THEN
	ii_accion = 5
END IF	
end event

type st_14 from statictext within w_info_repalletdeta_sag_nuevo
integer x = 823
integer y = 1772
integer width = 448
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
string text = "Tipo Actividad"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_15 from statictext within w_info_repalletdeta_sag_nuevo
integer x = 247
integer y = 1588
integer width = 1902
integer height = 148
boolean bringtotop = true
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

type cbx_1 from checkbox within w_info_repalletdeta_sag_nuevo
integer x = 544
integer y = 1624
integer width = 571
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
string text = "Supervisado"
end type

type cbx_2 from checkbox within w_info_repalletdeta_sag_nuevo
integer x = 1243
integer y = 1624
integer width = 654
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
string text = "No Supervisado"
end type

type st_19 from statictext within w_info_repalletdeta_sag_nuevo
integer x = 302
integer y = 1320
integer width = 562
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
string text = "Contraparte"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_contraparte from singlelineedit within w_info_repalletdeta_sag_nuevo
integer x = 901
integer y = 1296
integer width = 1193
integer height = 96
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
end type

type st_16 from statictext within w_info_repalletdeta_sag_nuevo
integer x = 247
integer y = 1880
integer width = 1902
integer height = 120
boolean bringtotop = true
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

type cbx_pda from radiobutton within w_info_repalletdeta_sag_nuevo
integer x = 366
integer y = 1020
integer width = 626
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
string text = "Repalletizado PDA"
boolean lefttext = true
end type

event clicked;IF This.Checked THEN
	cbx_soloinspec.Visible = False
	cbx_soloinspec.Checked = False
END IF	
end event

type cbx_csp from radiobutton within w_info_repalletdeta_sag_nuevo
integer x = 283
integer y = 1896
integer width = 731
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
string text = "Formato Incluye CSP"
boolean lefttext = true
end type

event clicked;IF This.Checked THEN
	cbx_soloinspec.Visible = False
	cbx_soloinspec.Checked = False
END IF	
end event

type cbx_nuevoformato from radiobutton within w_info_repalletdeta_sag_nuevo
integer x = 1294
integer y = 1004
integer width = 617
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Formato Nuevo"
boolean checked = true
boolean lefttext = true
end type

event clicked;IF This.Checked THEN
	cbx_soloinspec.Visible = True
	cbx_soloinspec.Checked = False
END IF	
end event

type cbx_soloinspec from checkbox within w_info_repalletdeta_sag_nuevo
integer x = 1303
integer y = 1900
integer width = 690
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
string text = "Solo Inpeccionados"
boolean lefttext = true
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_repalletdeta_sag_nuevo
event destroy ( )
integer x = 951
integer y = 476
integer height = 96
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_repalletdeta_sag_nuevo
event destroy ( )
integer x = 951
integer y = 580
integer height = 96
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

