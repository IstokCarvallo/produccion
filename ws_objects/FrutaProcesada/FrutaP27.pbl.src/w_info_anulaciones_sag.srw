$PBExportHeader$w_info_anulaciones_sag.srw
$PBExportComments$Informe de Revisión Repalletizajes.
forward
global type w_info_anulaciones_sag from w_para_informes
end type
type st_4 from statictext within w_info_anulaciones_sag
end type
type st_1 from statictext within w_info_anulaciones_sag
end type
type st_5 from statictext within w_info_anulaciones_sag
end type
type st_2 from statictext within w_info_anulaciones_sag
end type
type em_numero from editmask within w_info_anulaciones_sag
end type
type st_6 from statictext within w_info_anulaciones_sag
end type
type cb_buscarepa from commandbutton within w_info_anulaciones_sag
end type
type sle_fecha from singlelineedit within w_info_anulaciones_sag
end type
type st_3 from statictext within w_info_anulaciones_sag
end type
type st_7 from statictext within w_info_anulaciones_sag
end type
type em_programa from editmask within w_info_anulaciones_sag
end type
type em_registro from editmask within w_info_anulaciones_sag
end type
type st_8 from statictext within w_info_anulaciones_sag
end type
type st_11 from statictext within w_info_anulaciones_sag
end type
type st_12 from statictext within w_info_anulaciones_sag
end type
type em_observaciones from editmask within w_info_anulaciones_sag
end type
type rb_1 from radiobutton within w_info_anulaciones_sag
end type
type rb_2 from radiobutton within w_info_anulaciones_sag
end type
type st_10 from statictext within w_info_anulaciones_sag
end type
type ddlb_accion from dropdownlistbox within w_info_anulaciones_sag
end type
type st_14 from statictext within w_info_anulaciones_sag
end type
type cbx_archivo from checkbox within w_info_anulaciones_sag
end type
type st_13 from statictext within w_info_anulaciones_sag
end type
type em_condicion from editmask within w_info_anulaciones_sag
end type
type em_producto from editmask within w_info_anulaciones_sag
end type
type em_envase from editmask within w_info_anulaciones_sag
end type
type st_15 from statictext within w_info_anulaciones_sag
end type
type st_16 from statictext within w_info_anulaciones_sag
end type
type st_17 from statictext within w_info_anulaciones_sag
end type
type st_18 from statictext within w_info_anulaciones_sag
end type
type st_19 from statictext within w_info_anulaciones_sag
end type
type st_20 from statictext within w_info_anulaciones_sag
end type
type em_fecha from editmask within w_info_anulaciones_sag
end type
type st_21 from statictext within w_info_anulaciones_sag
end type
type em_contraparte from singlelineedit within w_info_anulaciones_sag
end type
type st_9 from statictext within w_info_anulaciones_sag
end type
type st_22 from statictext within w_info_anulaciones_sag
end type
type em_nlote from editmask within w_info_anulaciones_sag
end type
type st_23 from statictext within w_info_anulaciones_sag
end type
type em_causal from editmask within w_info_anulaciones_sag
end type
type rb_origen from radiobutton within w_info_anulaciones_sag
end type
type rb_forestal from radiobutton within w_info_anulaciones_sag
end type
type cbx_informe from radiobutton within w_info_anulaciones_sag
end type
type cbx_nuevo from radiobutton within w_info_anulaciones_sag
end type
type cbx_csp from radiobutton within w_info_anulaciones_sag
end type
type cbx_formato from radiobutton within w_info_anulaciones_sag
end type
type uo_selplantas from uo_seleccion_plantas within w_info_anulaciones_sag
end type
type uo_SelCliente from uo_seleccion_clientesprod within w_info_anulaciones_sag
end type
type r_1 from rectangle within w_info_anulaciones_sag
end type
end forward

global type w_info_anulaciones_sag from w_para_informes
integer x = 14
integer y = 32
integer width = 3195
integer height = 2548
string title = "Informe de Anulaciones S.A.G."
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
sle_fecha sle_fecha
st_3 st_3
st_7 st_7
em_programa em_programa
em_registro em_registro
st_8 st_8
st_11 st_11
st_12 st_12
em_observaciones em_observaciones
rb_1 rb_1
rb_2 rb_2
st_10 st_10
ddlb_accion ddlb_accion
st_14 st_14
cbx_archivo cbx_archivo
st_13 st_13
em_condicion em_condicion
em_producto em_producto
em_envase em_envase
st_15 st_15
st_16 st_16
st_17 st_17
st_18 st_18
st_19 st_19
st_20 st_20
em_fecha em_fecha
st_21 st_21
em_contraparte em_contraparte
st_9 st_9
st_22 st_22
em_nlote em_nlote
st_23 st_23
em_causal em_causal
rb_origen rb_origen
rb_forestal rb_forestal
cbx_informe cbx_informe
cbx_nuevo cbx_nuevo
cbx_csp cbx_csp
cbx_formato cbx_formato
uo_selplantas uo_selplantas
uo_SelCliente uo_SelCliente
r_1 r_1
end type
global w_info_anulaciones_sag w_info_anulaciones_sag

type variables
str_busqueda istr_busq
str_mant istr_mant
Integer		ii_accion
String			is_codsag

DataWindowChild	idwc_tipopro, idwc_especie
end variables

forward prototypes
public function string contraparte (integer ai_planta)
end prototypes

public function string contraparte (integer ai_planta);String	ls_nombre,ls_paterm,ls_mater,ls_cargos

SELECT tecn_nombre,tecn_apepat,tecn_apemat
INTO :ls_nombre,:ls_paterm,:ls_mater
FROM dbo.cargostecnicos
WHERE plde_codigo = :ai_Planta
AND	tecn_codigo = 1
AND	tecn_tprofe = 1;


ls_cargos = ls_nombre+' '+ls_paterm+' '+ls_mater
return ls_cargos




end function

on w_info_anulaciones_sag.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_5=create st_5
this.st_2=create st_2
this.em_numero=create em_numero
this.st_6=create st_6
this.cb_buscarepa=create cb_buscarepa
this.sle_fecha=create sle_fecha
this.st_3=create st_3
this.st_7=create st_7
this.em_programa=create em_programa
this.em_registro=create em_registro
this.st_8=create st_8
this.st_11=create st_11
this.st_12=create st_12
this.em_observaciones=create em_observaciones
this.rb_1=create rb_1
this.rb_2=create rb_2
this.st_10=create st_10
this.ddlb_accion=create ddlb_accion
this.st_14=create st_14
this.cbx_archivo=create cbx_archivo
this.st_13=create st_13
this.em_condicion=create em_condicion
this.em_producto=create em_producto
this.em_envase=create em_envase
this.st_15=create st_15
this.st_16=create st_16
this.st_17=create st_17
this.st_18=create st_18
this.st_19=create st_19
this.st_20=create st_20
this.em_fecha=create em_fecha
this.st_21=create st_21
this.em_contraparte=create em_contraparte
this.st_9=create st_9
this.st_22=create st_22
this.em_nlote=create em_nlote
this.st_23=create st_23
this.em_causal=create em_causal
this.rb_origen=create rb_origen
this.rb_forestal=create rb_forestal
this.cbx_informe=create cbx_informe
this.cbx_nuevo=create cbx_nuevo
this.cbx_csp=create cbx_csp
this.cbx_formato=create cbx_formato
this.uo_selplantas=create uo_selplantas
this.uo_SelCliente=create uo_SelCliente
this.r_1=create r_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_5
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_numero
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.cb_buscarepa
this.Control[iCurrent+8]=this.sle_fecha
this.Control[iCurrent+9]=this.st_3
this.Control[iCurrent+10]=this.st_7
this.Control[iCurrent+11]=this.em_programa
this.Control[iCurrent+12]=this.em_registro
this.Control[iCurrent+13]=this.st_8
this.Control[iCurrent+14]=this.st_11
this.Control[iCurrent+15]=this.st_12
this.Control[iCurrent+16]=this.em_observaciones
this.Control[iCurrent+17]=this.rb_1
this.Control[iCurrent+18]=this.rb_2
this.Control[iCurrent+19]=this.st_10
this.Control[iCurrent+20]=this.ddlb_accion
this.Control[iCurrent+21]=this.st_14
this.Control[iCurrent+22]=this.cbx_archivo
this.Control[iCurrent+23]=this.st_13
this.Control[iCurrent+24]=this.em_condicion
this.Control[iCurrent+25]=this.em_producto
this.Control[iCurrent+26]=this.em_envase
this.Control[iCurrent+27]=this.st_15
this.Control[iCurrent+28]=this.st_16
this.Control[iCurrent+29]=this.st_17
this.Control[iCurrent+30]=this.st_18
this.Control[iCurrent+31]=this.st_19
this.Control[iCurrent+32]=this.st_20
this.Control[iCurrent+33]=this.em_fecha
this.Control[iCurrent+34]=this.st_21
this.Control[iCurrent+35]=this.em_contraparte
this.Control[iCurrent+36]=this.st_9
this.Control[iCurrent+37]=this.st_22
this.Control[iCurrent+38]=this.em_nlote
this.Control[iCurrent+39]=this.st_23
this.Control[iCurrent+40]=this.em_causal
this.Control[iCurrent+41]=this.rb_origen
this.Control[iCurrent+42]=this.rb_forestal
this.Control[iCurrent+43]=this.cbx_informe
this.Control[iCurrent+44]=this.cbx_nuevo
this.Control[iCurrent+45]=this.cbx_csp
this.Control[iCurrent+46]=this.cbx_formato
this.Control[iCurrent+47]=this.uo_selplantas
this.Control[iCurrent+48]=this.uo_SelCliente
this.Control[iCurrent+49]=this.r_1
end on

on w_info_anulaciones_sag.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.em_numero)
destroy(this.st_6)
destroy(this.cb_buscarepa)
destroy(this.sle_fecha)
destroy(this.st_3)
destroy(this.st_7)
destroy(this.em_programa)
destroy(this.em_registro)
destroy(this.st_8)
destroy(this.st_11)
destroy(this.st_12)
destroy(this.em_observaciones)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.st_10)
destroy(this.ddlb_accion)
destroy(this.st_14)
destroy(this.cbx_archivo)
destroy(this.st_13)
destroy(this.em_condicion)
destroy(this.em_producto)
destroy(this.em_envase)
destroy(this.st_15)
destroy(this.st_16)
destroy(this.st_17)
destroy(this.st_18)
destroy(this.st_19)
destroy(this.st_20)
destroy(this.em_fecha)
destroy(this.st_21)
destroy(this.em_contraparte)
destroy(this.st_9)
destroy(this.st_22)
destroy(this.em_nlote)
destroy(this.st_23)
destroy(this.em_causal)
destroy(this.rb_origen)
destroy(this.rb_forestal)
destroy(this.cbx_informe)
destroy(this.cbx_nuevo)
destroy(this.cbx_csp)
destroy(this.cbx_formato)
destroy(this.uo_selplantas)
destroy(this.uo_SelCliente)
destroy(this.r_1)
end on

event open;call super::open;boolean lb_Cerrar 

If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True

IF lb_Cerrar Then
	Close (This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlantas.Inicia(gi_CodPlanta)
	
	is_codsag					=	String(uo_SelPlantas.CodigoSAG)
	
	ddlb_accion.SelectItem(1)
	ii_accion = 1
	
	em_fecha.Text = String(Today())
	em_contraparte.Text = Contraparte(uo_SelPlantas.Codigo)
	
	IF date(now()) > Date('2009-11-01') THEN
		cbx_informe.Checked = False
	ELSE
		cbx_informe.Checked = True
	END IF	
	
	istr_mant.argumento[1]		=	String(uo_SelPlantas.Codigo)
	istr_mant.argumento[2]		=	""
	istr_mant.argumento[3]		=	String(uo_SelCliente.Codigo)
	istr_mant.argumento[4]		=	'0'
	istr_mant.argumento[5]		=	uo_SelPlantas.Nombre
	istr_mant.argumento[11]	=	uo_SelPlantas.Region
	
	ids_archivo					=	CREATE	DataStore
	ids_archivo.DataObject	=	'dw_info_anulaciones_sag_nuevo_excel'
	ids_archivo.SetTransObject(sqlca)
	
	em_programa.text			=	'ORIGEN'
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_anulaciones_sag
end type

type st_computador from w_para_informes`st_computador within w_info_anulaciones_sag
integer x = 2587
integer y = 148
end type

type st_usuario from w_para_informes`st_usuario within w_info_anulaciones_sag
integer x = 2587
end type

type st_temporada from w_para_informes`st_temporada within w_info_anulaciones_sag
integer x = 2587
integer y = 4
end type

type p_logo from w_para_informes`p_logo within w_info_anulaciones_sag
end type

type st_titulo from w_para_informes`st_titulo within w_info_anulaciones_sag
integer width = 2464
string text = "Informe de Anulaciones S.A.G."
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_anulaciones_sag
string tag = "Imprimir Reporte"
integer x = 2843
integer y = 1464
integer taborder = 200
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	fila,li_cbx_planta,li_especie,li_numero, li_region, li_origen, li_forestal
Long		li_planta,li_cliente
String		ls_region, ls_observaciones, ls_Archivo, ls_Ruta

ls_observaciones	=	em_observaciones.Text

li_region	=	Integer(istr_mant.argumento[11])

IF li_region = 1 THEN ls_region = 'I Región'
IF li_region = 2 THEN ls_region = 'II Región'
IF li_region = 3 THEN ls_region = 'III Región'
IF li_region = 4 THEN ls_region = 'IV Región'
IF li_region = 5 THEN ls_region = 'V Región'
IF li_region = 6 THEN ls_region = 'VI Región'
IF li_region = 7 THEN ls_region = 'VII Región'
IF li_region = 8 THEN ls_region = 'VIII Región'
IF li_region = 9 THEN ls_region = 'IX Región'
IF li_region = 10 THEN ls_region = 'X Región'
IF li_region = 11 THEN ls_region = 'XI Región'
IF li_region = 12 THEN ls_region = 'XII Región'
IF li_region = 13 THEN ls_region = 'Región Metropolitana'


IF Long(istr_mant.argumento[2]) = 0 THEN
	MessageBox("Atención", "Anulación S.A.G. debe ser Superior a 0.~r~rIngrese otro Número.", Exclamation!, Ok!)
	em_numero.SetFocus()
ELSEIF cbx_archivo.Checked THEN
	ls_Archivo	=	"\Anulaciones-"+String(Long(istr_mant.argumento[2]))+".xls"	
	ids_archivo.Retrieve(Integer(istr_mant.Argumento[3]), Integer(istr_mant.Argumento[1]), &
									Long(istr_mant.Argumento[2]),em_causal.Text)
	
	RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
	ids_archivo.SaveAs(ls_Ruta + ls_Archivo,excel5!, True)	
	MessageBox("Atención","Archivo Formato Excel, Generado.")	
ELSE
	istr_info.titulo	= 'INFORME DE ANULACIONES S.A.G.'

	OpenWithParm(vinf, istr_info)
	
	IF cbx_csp.Checked THEN
		vinf.dw_1.DataObject = "dw_info_anulaciones_sag_formato_csp"
	ELSE
		IF cbx_informe.Checked THEN
			vinf.dw_1.DataObject = "dw_info_anulaciones_sag_nuevo"
		ELSE
			IF cbx_nuevo.Checked THEN
				vinf.dw_1.DataObject = "dw_info_anulaciones_sag_nuevo_formato_032013"//"dw_info_anulaciones_sag_nuevo_formato_2013"
			ELSE
				vinf.dw_1.DataObject = "dw_info_anulaciones_sag_nuevo_formato_032013"//"dw_info_anulaciones_sag_nuevo_formato"
			END IF	
		END IF
	END IF
	
	IF rb_origen.Checked THEN
		li_origen = 1
		em_programa.Text = "ORIGEN"
	END IF	
	IF rb_forestal.Checked THEN
		li_forestal = 1
		em_programa.Text = "FORESTAL"
	END IF
	
	vinf.dw_1.SetTransObject(sqlca)
	
	fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[3]), Integer(istr_mant.Argumento[1]), &
										Long(istr_mant.Argumento[2]),em_causal.Text,ii_accion,&
										em_condicion.Text,em_producto.Text,em_envase.Text,li_origen,li_forestal)
	IF fila = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
						"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
	ELSE
		F_Membrete(vinf.dw_1)
		
		vinf.dw_1.Modify("frigorifico.text = '" + istr_mant.Argumento[5] + "'")
		vinf.dw_1.Modify("registro.text = '" + em_registro.Text + "'")			
		vinf.dw_1.Modify("fecha.text = '" + String(Date(istr_mant.Argumento[10])) + "'")
		vinf.dw_1.Modify("anulacion.text = '" + istr_mant.Argumento[2] + "'")			
		vinf.dw_1.Modify("region.text = '" + ls_region + "'")	
		vinf.dw_1.Modify("t_observa.text = '" + ls_observaciones + "'")
		vinf.dw_1.Modify("codsag.text = '" + is_codsag + "'")			
		vinf.dw_1.Modify("t_contraparte.text = '" + em_contraparte.Text + "'")	
		vinf.dw_1.Modify("t_fecha.text = '" + em_fecha.Text + "'")	
		vinf.dw_1.Modify("t_programa.text = '" + em_programa.Text + "'")	
		vinf.dw_1.Modify("t_lote.text = '" + em_nlote.Text + "'")	
		vinf.dw_1.Modify("t_certificado.text = '" + em_registro.Text + "'")	
		
		IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_anulaciones_sag
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2834
integer y = 1780
integer taborder = 210
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_anulaciones_sag
integer x = 251
integer y = 420
integer width = 2464
integer height = 212
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

type st_1 from statictext within w_info_anulaciones_sag
integer x = 302
integer y = 544
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

type st_5 from statictext within w_info_anulaciones_sag
integer x = 251
integer y = 632
integer width = 2464
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

type st_2 from statictext within w_info_anulaciones_sag
integer x = 302
integer y = 660
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
string text = "N° Anulación"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_numero from editmask within w_info_anulaciones_sag
integer x = 923
integer y = 644
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
string mask = "#####"
end type

event modified;Long		ll_numero
Date		ld_fzarpe


ll_numero		=	Long(This.Text)
	
SELECT	max(inpd_fechaa)
  INTO	:ld_fzarpe
  FROM	dbo.INSPECPALDET
 WHERE	clie_codigo	=	:uo_SelCliente.Codigo
	AND	plde_codigo =	:uo_SelPlantas.Codigo
	AND   inpd_nroanu =  :ll_numero;
				
IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla INSPECPALDET")
		em_numero.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Anulación Indicada.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		em_numero.SetFocus()
	ELSE
		istr_mant.argumento[10]	=	String(ld_fzarpe)
		istr_mant.argumento[2]	=	This.Text		
		sle_fecha.text				= 	String(ld_fzarpe)
	END IF

end event

type st_6 from statictext within w_info_anulaciones_sag
integer x = 302
integer y = 452
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

type cb_buscarepa from commandbutton within w_info_anulaciones_sag
integer x = 1330
integer y = 648
integer width = 91
integer height = 84
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;istr_busq.argum[1]	=	String(uo_SelCliente.Codigo)
istr_busq.argum[2]	=	String(uo_SelPlantas.Codigo)
istr_busq.argum[3]	=	'1'

OpenWithParm(w_busc_inspecpaldet_anula, istr_busq)

istr_busq	       = Message.PowerObjectParm

IF istr_busq.argum[5] <> "" THEN
	em_numero.Text				= 	istr_busq.argum[5]
	istr_mant.argumento[2]	= 	istr_busq.argum[5]
	istr_mant.argumento[10]	=	istr_busq.argum[10]
	sle_fecha.Text				=	istr_busq.argum[10]
ELSE
	em_numero.SetFocus()
END IF
end event

type sle_fecha from singlelineedit within w_info_anulaciones_sag
integer x = 1431
integer y = 648
integer width = 1211
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
borderstyle borderstyle = stylelowered!
end type

type st_3 from statictext within w_info_anulaciones_sag
integer x = 251
integer y = 752
integer width = 2464
integer height = 340
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

type st_7 from statictext within w_info_anulaciones_sag
integer x = 302
integer y = 784
integer width = 622
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
string text = "Programa"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_programa from editmask within w_info_anulaciones_sag
event getfocus pbm_ensetfocus
boolean visible = false
integer x = 928
integer y = 776
integer width = 1079
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

type em_registro from editmask within w_info_anulaciones_sag
event getfocus pbm_ensetfocus
integer x = 1298
integer y = 928
integer width = 1367
integer height = 120
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

type st_8 from statictext within w_info_anulaciones_sag
integer x = 302
integer y = 916
integer width = 997
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
string text = "Cantidad de certifiados que anula"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_anulaciones_sag
integer x = 251
integer y = 1092
integer width = 2464
integer height = 492
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

type st_12 from statictext within w_info_anulaciones_sag
integer x = 315
integer y = 1104
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

type em_observaciones from editmask within w_info_anulaciones_sag
integer x = 320
integer y = 1164
integer width = 2341
integer height = 96
integer taborder = 80
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

type rb_1 from radiobutton within w_info_anulaciones_sag
boolean visible = false
integer x = 1408
integer y = 2480
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Antiguo"
boolean checked = true
end type

event clicked;cbx_archivo.visible = False
end event

type rb_2 from radiobutton within w_info_anulaciones_sag
boolean visible = false
integer x = 1842
integer y = 2488
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Nuevo"
boolean checked = true
end type

event clicked;cbx_archivo.visible = True
end event

type st_10 from statictext within w_info_anulaciones_sag
integer x = 741
integer y = 1740
integer width = 462
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

type ddlb_accion from dropdownlistbox within w_info_anulaciones_sag
integer x = 1243
integer y = 1724
integer width = 910
integer height = 532
integer taborder = 180
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean sorted = false
boolean hscrollbar = true
boolean vscrollbar = true
string item[] = {"Inspección","Tratamiento","Repaletizaje","Interplanta","Otro","",""}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_accion = index
end event

type st_14 from statictext within w_info_anulaciones_sag
integer x = 302
integer y = 976
integer width = 718
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
string text = "o Devuelve"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_archivo from checkbox within w_info_anulaciones_sag
boolean visible = false
integer x = 302
integer y = 1608
integer width = 320
integer height = 76
integer taborder = 120
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

type st_13 from statictext within w_info_anulaciones_sag
integer x = 251
integer y = 1716
integer width = 2464
integer height = 108
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_condicion from editmask within w_info_anulaciones_sag
integer x = 923
integer y = 1268
integer width = 1728
integer height = 96
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
string text = "Fresco"
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type em_producto from editmask within w_info_anulaciones_sag
integer x = 923
integer y = 1372
integer width = 1728
integer height = 96
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
string text = "fruta"
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type em_envase from editmask within w_info_anulaciones_sag
integer x = 923
integer y = 1476
integer width = 1728
integer height = 96
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
string text = "cajas"
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type st_15 from statictext within w_info_anulaciones_sag
integer x = 315
integer y = 1300
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
string text = "Condición"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_16 from statictext within w_info_anulaciones_sag
integer x = 315
integer y = 1404
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
string text = "Producto"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_17 from statictext within w_info_anulaciones_sag
integer x = 315
integer y = 1508
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
string text = "Tipo Envase"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_18 from statictext within w_info_anulaciones_sag
integer x = 251
integer y = 1824
integer width = 2464
integer height = 116
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_19 from statictext within w_info_anulaciones_sag
integer x = 297
integer y = 1984
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

type st_20 from statictext within w_info_anulaciones_sag
integer x = 297
integer y = 2084
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
string text = "Fecha Entrega"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fecha from editmask within w_info_anulaciones_sag
integer x = 923
integer y = 2052
integer width = 462
integer height = 96
integer taborder = 150
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
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_21 from statictext within w_info_anulaciones_sag
integer x = 251
integer y = 1940
integer width = 2464
integer height = 332
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_contraparte from singlelineedit within w_info_anulaciones_sag
integer x = 923
integer y = 1952
integer width = 1728
integer height = 96
integer taborder = 190
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

type st_9 from statictext within w_info_anulaciones_sag
integer x = 251
integer y = 1584
integer width = 2464
integer height = 128
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_22 from statictext within w_info_anulaciones_sag
integer x = 311
integer y = 1852
integer width = 462
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
string text = "N° Lote"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_nlote from editmask within w_info_anulaciones_sag
integer x = 923
integer y = 1832
integer width = 1728
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
long backcolor = 16777215
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type st_23 from statictext within w_info_anulaciones_sag
integer x = 297
integer y = 2180
integer width = 997
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
string text = "Causal de Anulación"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_causal from editmask within w_info_anulaciones_sag
event getfocus pbm_ensetfocus
integer x = 923
integer y = 2152
integer width = 1737
integer height = 88
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

type rb_origen from radiobutton within w_info_anulaciones_sag
integer x = 933
integer y = 784
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
string text = "ORIGEN"
boolean checked = true
end type

event clicked;em_programa.text			=	'ORIGEN'
end event

type rb_forestal from radiobutton within w_info_anulaciones_sag
integer x = 1481
integer y = 784
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
string text = "FORESTAL"
end type

event clicked;em_programa.text			=	'FORESTAL'
end event

type cbx_informe from radiobutton within w_info_anulaciones_sag
boolean visible = false
integer x = 1947
integer y = 1604
integer width = 567
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Formato Antiguo"
boolean automatic = false
end type

type cbx_nuevo from radiobutton within w_info_anulaciones_sag
boolean visible = false
integer x = 1513
integer y = 1608
integer width = 686
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
string text = "Formato Marzo 2013"
boolean automatic = false
end type

type cbx_csp from radiobutton within w_info_anulaciones_sag
boolean visible = false
integer x = 1819
integer y = 1612
integer width = 695
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
string text = "Formato Incluye CSP"
boolean automatic = false
end type

type cbx_formato from radiobutton within w_info_anulaciones_sag
integer x = 919
integer y = 1600
integer width = 526
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
string text = "Formato Nuevo"
boolean checked = true
end type

type uo_selplantas from uo_seleccion_plantas within w_info_anulaciones_sag
integer x = 923
integer y = 536
integer height = 100
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;istr_mant.argumento[1]		=	String(This.Codigo)
istr_mant.Argumento[5]		=	This.Nombre
istr_mant.Argumento[11]	=	This.Region
is_codsag						=	String(This.CodigoSAG)
end event

type uo_SelCliente from uo_seleccion_clientesprod within w_info_anulaciones_sag
event destroy ( )
integer x = 923
integer y = 436
integer height = 100
integer taborder = 20
boolean bringtotop = true
end type

on uo_SelCliente.destroy
call uo_seleccion_clientesprod::destroy
end on

event ue_cambio;call super::ue_cambio;istr_mant.argumento[3]	=	String(This.Codigo)
	
end event

type r_1 from rectangle within w_info_anulaciones_sag
integer linethickness = 4
long fillcolor = 16777215
integer x = 873
integer y = 788
integer width = 1243
integer height = 72
end type

