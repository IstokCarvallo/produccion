$PBExportHeader$w_info_devoluciones_sag.srw
$PBExportComments$Informe de Revisión Repalletizajes.
forward
global type w_info_devoluciones_sag from w_para_informes
end type
type st_4 from statictext within w_info_devoluciones_sag
end type
type st_1 from statictext within w_info_devoluciones_sag
end type
type st_5 from statictext within w_info_devoluciones_sag
end type
type st_2 from statictext within w_info_devoluciones_sag
end type
type em_numero from editmask within w_info_devoluciones_sag
end type
type st_6 from statictext within w_info_devoluciones_sag
end type
type cb_buscarepa from commandbutton within w_info_devoluciones_sag
end type
type sle_fecha from singlelineedit within w_info_devoluciones_sag
end type
type st_3 from statictext within w_info_devoluciones_sag
end type
type st_7 from statictext within w_info_devoluciones_sag
end type
type em_causal from editmask within w_info_devoluciones_sag
end type
type em_registro from editmask within w_info_devoluciones_sag
end type
type st_8 from statictext within w_info_devoluciones_sag
end type
type st_11 from statictext within w_info_devoluciones_sag
end type
type st_12 from statictext within w_info_devoluciones_sag
end type
type em_observaciones from editmask within w_info_devoluciones_sag
end type
type st_9 from statictext within w_info_devoluciones_sag
end type
type rb_1 from radiobutton within w_info_devoluciones_sag
end type
type rb_2 from radiobutton within w_info_devoluciones_sag
end type
type st_10 from statictext within w_info_devoluciones_sag
end type
type ddlb_accion from dropdownlistbox within w_info_devoluciones_sag
end type
type st_13 from statictext within w_info_devoluciones_sag
end type
type st_15 from statictext within w_info_devoluciones_sag
end type
type em_condicion from editmask within w_info_devoluciones_sag
end type
type st_16 from statictext within w_info_devoluciones_sag
end type
type em_producto from editmask within w_info_devoluciones_sag
end type
type st_17 from statictext within w_info_devoluciones_sag
end type
type em_envase from editmask within w_info_devoluciones_sag
end type
type st_22 from statictext within w_info_devoluciones_sag
end type
type st_19 from statictext within w_info_devoluciones_sag
end type
type em_contraparte from singlelineedit within w_info_devoluciones_sag
end type
type st_20 from statictext within w_info_devoluciones_sag
end type
type em_fecha from editmask within w_info_devoluciones_sag
end type
type st_18 from statictext within w_info_devoluciones_sag
end type
type st_14 from statictext within w_info_devoluciones_sag
end type
type st_21 from statictext within w_info_devoluciones_sag
end type
type em_programa from editmask within w_info_devoluciones_sag
end type
type st_23 from statictext within w_info_devoluciones_sag
end type
type em_nlote from editmask within w_info_devoluciones_sag
end type
type st_24 from statictext within w_info_devoluciones_sag
end type
type cbx_origen from radiobutton within w_info_devoluciones_sag
end type
type cbx_forestal from radiobutton within w_info_devoluciones_sag
end type
type cbx_info from radiobutton within w_info_devoluciones_sag
end type
type cbx_nuevo from radiobutton within w_info_devoluciones_sag
end type
type cbx_nuevoformato from radiobutton within w_info_devoluciones_sag
end type
type cbx_csp from radiobutton within w_info_devoluciones_sag
end type
type uo_selplantas from uo_seleccion_plantas within w_info_devoluciones_sag
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_devoluciones_sag
end type
end forward

global type w_info_devoluciones_sag from w_para_informes
integer x = 14
integer y = 32
integer width = 2606
integer height = 2420
string title = "Informe de Devoluciones S.A.G."
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
em_causal em_causal
em_registro em_registro
st_8 st_8
st_11 st_11
st_12 st_12
em_observaciones em_observaciones
st_9 st_9
rb_1 rb_1
rb_2 rb_2
st_10 st_10
ddlb_accion ddlb_accion
st_13 st_13
st_15 st_15
em_condicion em_condicion
st_16 st_16
em_producto em_producto
st_17 st_17
em_envase em_envase
st_22 st_22
st_19 st_19
em_contraparte em_contraparte
st_20 st_20
em_fecha em_fecha
st_18 st_18
st_14 st_14
st_21 st_21
em_programa em_programa
st_23 st_23
em_nlote em_nlote
st_24 st_24
cbx_origen cbx_origen
cbx_forestal cbx_forestal
cbx_info cbx_info
cbx_nuevo cbx_nuevo
cbx_nuevoformato cbx_nuevoformato
cbx_csp cbx_csp
uo_selplantas uo_selplantas
uo_selcliente uo_selcliente
end type
global w_info_devoluciones_sag w_info_devoluciones_sag

type variables
str_busqueda istr_busq
str_mant istr_mant

Integer	ii_accion
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

on w_info_devoluciones_sag.create
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
this.em_causal=create em_causal
this.em_registro=create em_registro
this.st_8=create st_8
this.st_11=create st_11
this.st_12=create st_12
this.em_observaciones=create em_observaciones
this.st_9=create st_9
this.rb_1=create rb_1
this.rb_2=create rb_2
this.st_10=create st_10
this.ddlb_accion=create ddlb_accion
this.st_13=create st_13
this.st_15=create st_15
this.em_condicion=create em_condicion
this.st_16=create st_16
this.em_producto=create em_producto
this.st_17=create st_17
this.em_envase=create em_envase
this.st_22=create st_22
this.st_19=create st_19
this.em_contraparte=create em_contraparte
this.st_20=create st_20
this.em_fecha=create em_fecha
this.st_18=create st_18
this.st_14=create st_14
this.st_21=create st_21
this.em_programa=create em_programa
this.st_23=create st_23
this.em_nlote=create em_nlote
this.st_24=create st_24
this.cbx_origen=create cbx_origen
this.cbx_forestal=create cbx_forestal
this.cbx_info=create cbx_info
this.cbx_nuevo=create cbx_nuevo
this.cbx_nuevoformato=create cbx_nuevoformato
this.cbx_csp=create cbx_csp
this.uo_selplantas=create uo_selplantas
this.uo_selcliente=create uo_selcliente
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
this.Control[iCurrent+11]=this.em_causal
this.Control[iCurrent+12]=this.em_registro
this.Control[iCurrent+13]=this.st_8
this.Control[iCurrent+14]=this.st_11
this.Control[iCurrent+15]=this.st_12
this.Control[iCurrent+16]=this.em_observaciones
this.Control[iCurrent+17]=this.st_9
this.Control[iCurrent+18]=this.rb_1
this.Control[iCurrent+19]=this.rb_2
this.Control[iCurrent+20]=this.st_10
this.Control[iCurrent+21]=this.ddlb_accion
this.Control[iCurrent+22]=this.st_13
this.Control[iCurrent+23]=this.st_15
this.Control[iCurrent+24]=this.em_condicion
this.Control[iCurrent+25]=this.st_16
this.Control[iCurrent+26]=this.em_producto
this.Control[iCurrent+27]=this.st_17
this.Control[iCurrent+28]=this.em_envase
this.Control[iCurrent+29]=this.st_22
this.Control[iCurrent+30]=this.st_19
this.Control[iCurrent+31]=this.em_contraparte
this.Control[iCurrent+32]=this.st_20
this.Control[iCurrent+33]=this.em_fecha
this.Control[iCurrent+34]=this.st_18
this.Control[iCurrent+35]=this.st_14
this.Control[iCurrent+36]=this.st_21
this.Control[iCurrent+37]=this.em_programa
this.Control[iCurrent+38]=this.st_23
this.Control[iCurrent+39]=this.em_nlote
this.Control[iCurrent+40]=this.st_24
this.Control[iCurrent+41]=this.cbx_origen
this.Control[iCurrent+42]=this.cbx_forestal
this.Control[iCurrent+43]=this.cbx_info
this.Control[iCurrent+44]=this.cbx_nuevo
this.Control[iCurrent+45]=this.cbx_nuevoformato
this.Control[iCurrent+46]=this.cbx_csp
this.Control[iCurrent+47]=this.uo_selplantas
this.Control[iCurrent+48]=this.uo_selcliente
end on

on w_info_devoluciones_sag.destroy
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
destroy(this.em_causal)
destroy(this.em_registro)
destroy(this.st_8)
destroy(this.st_11)
destroy(this.st_12)
destroy(this.em_observaciones)
destroy(this.st_9)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.st_10)
destroy(this.ddlb_accion)
destroy(this.st_13)
destroy(this.st_15)
destroy(this.em_condicion)
destroy(this.st_16)
destroy(this.em_producto)
destroy(this.st_17)
destroy(this.em_envase)
destroy(this.st_22)
destroy(this.st_19)
destroy(this.em_contraparte)
destroy(this.st_20)
destroy(this.em_fecha)
destroy(this.st_18)
destroy(this.st_14)
destroy(this.st_21)
destroy(this.em_programa)
destroy(this.st_23)
destroy(this.em_nlote)
destroy(this.st_24)
destroy(this.cbx_origen)
destroy(this.cbx_forestal)
destroy(this.cbx_info)
destroy(this.cbx_nuevo)
destroy(this.cbx_nuevoformato)
destroy(this.cbx_csp)
destroy(this.uo_selplantas)
destroy(this.uo_selcliente)
end on

event open;call super::open;Boolean	lb_Cerrar 

If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True


If lb_Cerrar Then
	Close(This)
Else	
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlantas.Inicia(gi_CodPlanta)
		
	ddlb_accion.SelectItem(1)
	
	ii_accion = 1
	
	IF date(now()) > Date('2009-11-01') THEN
		cbx_info.Checked = False
	ELSE
		cbx_info.Checked = True
	END IF	
	
	em_fecha.Text = String(Today())
	
	em_contraparte.Text = contraparte(uo_SelPlantas.Codigo)
	
	istr_mant.argumento[1]		=	String(uo_SelPlantas.Codigo)
	istr_mant.argumento[2]		=	""
	istr_mant.argumento[3]		=	String(uo_SelCliente.Codigo)
	istr_mant.argumento[4]		=	'0'
	istr_mant.argumento[5]		=	uo_SelPlantas.Nombre
	istr_mant.argumento[11]	=	uo_SelPlantas.Region
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_devoluciones_sag
end type

type st_computador from w_para_informes`st_computador within w_info_devoluciones_sag
end type

type st_usuario from w_para_informes`st_usuario within w_info_devoluciones_sag
end type

type st_temporada from w_para_informes`st_temporada within w_info_devoluciones_sag
end type

type p_logo from w_para_informes`p_logo within w_info_devoluciones_sag
end type

type st_titulo from w_para_informes`st_titulo within w_info_devoluciones_sag
integer x = 238
integer y = 276
integer width = 1902
string text = "Informe de Devoluciones S.A.G."
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_devoluciones_sag
string tag = "Imprimir Reporte"
integer x = 2245
integer y = 1472
integer taborder = 70
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	fila,li_cbx_planta,li_especie,li_numero, li_region, li_origen,li_forestal
Long		li_planta,li_cliente
String	ls_region, ls_observaciones

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
		MessageBox("Atención", "Devolución S.A.G. debe ser Superior a 0.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		em_numero.SetFocus()

ELSE
		istr_info.titulo	= 'INFORME DE DEVOLUCIONES S.A.G.'

		OpenWithParm(vinf, istr_info)
		
		IF cbx_csp.Checked THEN
			vinf.dw_1.DataObject = "dw_info_devol_sag_nuevo_formato_csp"
			
		ELSEIF cbx_nuevo.Checked THEN	
			vinf.dw_1.DataObject = "dw_info_devol_sag_nuevo_febrero032013"//"dw_info_devol_sag_nuevo_febrero2013"
		ELSE
			IF cbx_info.Checked THEN
				vinf.dw_1.DataObject = "dw_info_devoluciones_sag_nuevo"
			ELSE
				vinf.dw_1.DataObject = "dw_info_devol_sag_nuevo_febrero032013"//"dw_info_devol_sag_nuevo_formato_2"//dw_info_devoluciones_sag_nuevo_formato
			END IF
		END IF	
		
		
		
		vinf.dw_1.SetTransObject(sqlca)
		
		IF cbx_origen.Checked THEN
			li_origen = 1
			em_programa.Text = "ORIGEN"
		END IF	
		IF cbx_forestal.Checked THEN
			li_forestal = 1
			em_programa.Text = "FORESTAL"
		END IF
		
		fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[3]), &
											Integer(istr_mant.Argumento[1]), &
											Long(istr_mant.Argumento[2]),em_causal.Text,ii_accion,&
											em_condicion.Text,em_producto.Text,em_envase.Text,li_origen,li_forestal)
											
		IF fila = -1 THEN
			MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
							"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
		
		ELSEIF fila = 0 THEN
			MessageBox( "No Existe información", "No existe información para este informe.", &
							 StopSign!, Ok!)
		
		ELSE
			F_Membrete(vinf.dw_1)
			
			vinf.dw_1.Modify("frigorifico.text = '" + istr_mant.Argumento[5] + "'")
			vinf.dw_1.Modify("registro.text = '" + em_registro.Text + "'")			
			vinf.dw_1.Modify("t_contraparte.text = '" + em_contraparte.Text + "'")	
			vinf.dw_1.Modify("t_fecha.text = '" + em_fecha.Text + "'")	
			vinf.dw_1.Modify("anulacion.text = '" + istr_mant.Argumento[2] + "'")			
			vinf.dw_1.Modify("region.text = '" + ls_region + "'")	
			vinf.dw_1.Modify("t_observa.text = '" + ls_observaciones + "'")
			vinf.dw_1.Modify("t_certificado.text = '" + em_registro.Text + "'")		
			vinf.dw_1.Modify("t_lote.text = '" + em_nlote.Text + "'")	
			vinf.dw_1.Modify("t_programa.text = '" + em_programa.Text + "'")			
			
			vinf.dw_1.Modify("codsag.text = '" + String(uo_SelPlantas.CodigoSAG) + "'")
			
			IF gs_Ambiente <> 'Windows' THEN
				F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
			END IF
		END IF
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_devoluciones_sag
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2245
integer y = 1784
integer taborder = 80
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_devoluciones_sag
integer x = 238
integer y = 416
integer width = 1902
integer height = 216
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

type st_1 from statictext within w_info_devoluciones_sag
integer x = 279
integer y = 540
integer width = 462
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_devoluciones_sag
integer x = 238
integer y = 632
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

type st_2 from statictext within w_info_devoluciones_sag
integer x = 279
integer y = 664
integer width = 517
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "N° Devolución"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_numero from editmask within w_info_devoluciones_sag
integer x = 910
integer y = 648
integer width = 393
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
end type

event modified;Long		ll_numero
Date		ld_fzarpe

ll_numero		=	Long(This.Text)
	
SELECT	max(inpd_fecdev)
  INTO	:ld_fzarpe
  FROM	dba.INSPECPALDET
 WHERE	clie_codigo	=	:uo_SelCliente.Codigo
	AND	plde_codigo =	:uo_SelPlantas.Codigo
	AND   inpd_nrodev =  :ll_numero;
				
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla INSPECPALDET")
	em_numero.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "No existe Devolución Indicada.~r~rIngrese otro Número.", Exclamation!, Ok!)
	em_numero.SetFocus()
ELSE
	istr_mant.argumento[10]	=	String(ld_fzarpe)
	istr_mant.argumento[2]	=	This.Text		
	sle_fecha.text				= 	String(ld_fzarpe)
END IF

end event

type st_6 from statictext within w_info_devoluciones_sag
integer x = 279
integer y = 452
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type cb_buscarepa from commandbutton within w_info_devoluciones_sag
integer x = 1317
integer y = 652
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

OpenWithParm(w_busc_inspecpaldet_devolucion, istr_busq)

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

type sle_fecha from singlelineedit within w_info_devoluciones_sag
integer x = 1417
integer y = 652
integer width = 690
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

type st_3 from statictext within w_info_devoluciones_sag
integer x = 238
integer y = 760
integer width = 1902
integer height = 284
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

type st_7 from statictext within w_info_devoluciones_sag
integer x = 279
integer y = 780
integer width = 645
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Causal de Devolución"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_causal from editmask within w_info_devoluciones_sag
event getfocus pbm_ensetfocus
integer x = 910
integer y = 772
integer width = 1193
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

type em_registro from editmask within w_info_devoluciones_sag
event getfocus pbm_ensetfocus
integer x = 1349
integer y = 872
integer width = 759
integer height = 120
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

type st_8 from statictext within w_info_devoluciones_sag
integer x = 279
integer y = 880
integer width = 1042
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cantidad de Certificados que Anula"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_devoluciones_sag
integer x = 238
integer y = 1044
integer width = 1902
integer height = 200
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

type st_12 from statictext within w_info_devoluciones_sag
integer x = 279
integer y = 1060
integer width = 640
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Observaciones S.A.G."
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_observaciones from editmask within w_info_devoluciones_sag
integer x = 311
integer y = 1124
integer width = 1801
integer height = 96
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type st_9 from statictext within w_info_devoluciones_sag
integer x = 238
integer y = 1244
integer width = 1902
integer height = 176
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

type rb_1 from radiobutton within w_info_devoluciones_sag
boolean visible = false
integer x = 142
integer y = 2376
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
end type

type rb_2 from radiobutton within w_info_devoluciones_sag
boolean visible = false
integer x = 805
integer y = 2376
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

type st_10 from statictext within w_info_devoluciones_sag
integer x = 873
integer y = 1300
integer width = 430
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tipo Actividad"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type ddlb_accion from dropdownlistbox within w_info_devoluciones_sag
integer x = 1312
integer y = 1276
integer width = 805
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

type st_13 from statictext within w_info_devoluciones_sag
integer x = 279
integer y = 944
integer width = 873
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "o Devuelve"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_15 from statictext within w_info_devoluciones_sag
integer x = 297
integer y = 1464
integer width = 562
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Condición"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_condicion from editmask within w_info_devoluciones_sag
integer x = 905
integer y = 1432
integer width = 1193
integer height = 96
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "Fresco"
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type st_16 from statictext within w_info_devoluciones_sag
integer x = 297
integer y = 1568
integer width = 562
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Producto"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_producto from editmask within w_info_devoluciones_sag
integer x = 905
integer y = 1536
integer width = 1193
integer height = 96
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "fruta"
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type st_17 from statictext within w_info_devoluciones_sag
integer x = 297
integer y = 1672
integer width = 562
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tipo Envase"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_envase from editmask within w_info_devoluciones_sag
integer x = 910
integer y = 1640
integer width = 1193
integer height = 96
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "cajas"
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type st_22 from statictext within w_info_devoluciones_sag
integer x = 297
integer y = 1920
integer width = 462
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Programa"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_19 from statictext within w_info_devoluciones_sag
integer x = 297
integer y = 2052
integer width = 562
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Contraparte"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_contraparte from singlelineedit within w_info_devoluciones_sag
integer x = 910
integer y = 2020
integer width = 1193
integer height = 96
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
end type

type st_20 from statictext within w_info_devoluciones_sag
integer x = 297
integer y = 2156
integer width = 562
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Entrega"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fecha from editmask within w_info_devoluciones_sag
integer x = 910
integer y = 2120
integer width = 462
integer height = 96
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_18 from statictext within w_info_devoluciones_sag
integer x = 238
integer y = 1908
integer width = 1902
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

type st_14 from statictext within w_info_devoluciones_sag
integer x = 238
integer y = 1420
integer width = 1902
integer height = 352
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

type st_21 from statictext within w_info_devoluciones_sag
integer x = 238
integer y = 2012
integer width = 1902
integer height = 236
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

type em_programa from editmask within w_info_devoluciones_sag
event getfocus pbm_ensetfocus
boolean visible = false
integer x = 2231
integer y = 996
integer width = 1079
integer height = 92
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string displaydata = "$"
end type

type st_23 from statictext within w_info_devoluciones_sag
integer x = 297
integer y = 1804
integer width = 462
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "N° Lote"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_nlote from editmask within w_info_devoluciones_sag
integer x = 910
integer y = 1784
integer width = 1193
integer height = 96
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type st_24 from statictext within w_info_devoluciones_sag
integer x = 238
integer y = 1772
integer width = 1902
integer height = 132
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

type cbx_origen from radiobutton within w_info_devoluciones_sag
integer x = 919
integer y = 1920
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
long backcolor = 553648127
string text = "ORIGEN"
boolean checked = true
end type

event clicked;em_programa.text			=	'ORIGEN'
end event

type cbx_forestal from radiobutton within w_info_devoluciones_sag
integer x = 1467
integer y = 1920
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
long backcolor = 553648127
string text = "FORESTAL"
end type

event clicked;em_programa.text			=	'FORESTAL'
end event

type cbx_info from radiobutton within w_info_devoluciones_sag
boolean visible = false
integer x = 279
integer y = 1256
integer width = 576
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Formato Antiguo"
boolean lefttext = true
end type

type cbx_nuevo from radiobutton within w_info_devoluciones_sag
boolean visible = false
integer x = 2048
integer y = 1284
integer width = 667
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Formato Marzo 2013"
boolean lefttext = true
end type

type cbx_nuevoformato from radiobutton within w_info_devoluciones_sag
integer x = 279
integer y = 1296
integer width = 576
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Formato Nuevo"
boolean checked = true
boolean lefttext = true
end type

type cbx_csp from radiobutton within w_info_devoluciones_sag
integer x = 1422
integer y = 2132
integer width = 695
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Formato Incluye CSP"
boolean lefttext = true
end type

type uo_selplantas from uo_seleccion_plantas within w_info_devoluciones_sag
event destroy ( )
integer x = 905
integer y = 528
integer height = 96
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;istr_mant.argumento[1]		=	String(This.Codigo)
istr_mant.Argumento[5]		=	This.Nombre
istr_mant.Argumento[11]	=	This.Region

end event

type uo_selcliente from uo_seleccion_clientesprod within w_info_devoluciones_sag
event destroy ( )
integer x = 905
integer y = 436
integer height = 96
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

