$PBExportHeader$w_info_repalletdeta_sag.srw
$PBExportComments$Informe de Revisión Repalletizajes.
forward
global type w_info_repalletdeta_sag from w_para_informes
end type
type st_4 from statictext within w_info_repalletdeta_sag
end type
type st_1 from statictext within w_info_repalletdeta_sag
end type
type st_5 from statictext within w_info_repalletdeta_sag
end type
type st_2 from statictext within w_info_repalletdeta_sag
end type
type em_numero from editmask within w_info_repalletdeta_sag
end type
type dw_2 from datawindow within w_info_repalletdeta_sag
end type
type st_6 from statictext within w_info_repalletdeta_sag
end type
type dw_1 from datawindow within w_info_repalletdeta_sag
end type
type cb_buscarepa from commandbutton within w_info_repalletdeta_sag
end type
type st_7 from statictext within w_info_repalletdeta_sag
end type
type st_8 from statictext within w_info_repalletdeta_sag
end type
type em_repasag from editmask within w_info_repalletdeta_sag
end type
type sle_fecha from singlelineedit within w_info_repalletdeta_sag
end type
type em_cambio from editmask within w_info_repalletdeta_sag
end type
type st_3 from statictext within w_info_repalletdeta_sag
end type
type sle_fechacambio from singlelineedit within w_info_repalletdeta_sag
end type
end forward

global type w_info_repalletdeta_sag from w_para_informes
integer x = 14
integer y = 32
integer width = 2725
integer height = 1604
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
dw_2 dw_2
st_6 st_6
dw_1 dw_1
cb_buscarepa cb_buscarepa
st_7 st_7
st_8 st_8
em_repasag em_repasag
sle_fecha sle_fecha
em_cambio em_cambio
st_3 st_3
sle_fechacambio sle_fechacambio
end type
global w_info_repalletdeta_sag w_info_repalletdeta_sag

type variables
Date			id_FechaAltu

str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta,idwc_especie
end variables

forward prototypes
public function boolean noexistefolioaltura (long al_numero)
end prototypes

public function boolean noexistefolioaltura (long al_numero);Integer	li_Cliente, li_Planta, li_CantTarjasCambio
Long		li_cantidad  //ll_Numero

li_Cliente	=	dw_2.Object.clie_codigo[1]
li_Planta	=	dw_1.Object.plde_codigo[1]
//ll_Numero		=	Long(istr_mant.argumento[2])
	
SELECT	altu_fecmov
	INTO	:id_FechaAltu
	FROM	dba.ALPALLETENCAB
	WHERE	clie_codigo	=	:li_Cliente
	AND	plde_codigo	=	:li_Planta
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
		FROM	dba.ALPALLETFRUTA
		WHERE	clie_codigo	=	:li_Cliente
		AND	plde_codigo	=	:li_Planta
		AND	altu_numero	=	:al_Numero;
//		AND	altu_numero	=	:ll_Numero;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Repalletizajes")
			
			RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Error", "Número de Cambio de Altura no tiene Detalle.~r~r" + &
						"Consulte con Encargado.", Exclamation!, Ok!)
			
			RETURN True
//	ELSE
//		SELECT	Count(paen_numero)
//			INTO	:ii_CantParticipaCambio
//			FROM	dba.ALPALLETFRUTA
//			WHERE	clie_codigo	=	:li_Cliente
//			AND	plde_codigo	=	:li_Planta
//			AND	altu_numero	=	:ll_Numero;
//		
//		SELECT	Count(rep.paen_numero)
//			INTO	:ii_CantInspecCambio
//			FROM	dba.ALPALLETFRUTA as rep, dba.PALLETENCAB as pae
//			WHERE	rep.clie_codigo	=	:li_Cliente
//			AND	rep.plde_codigo	=	:li_Planta
//			AND	rep.altu_numero	=	:ll_Numero
//			AND	pae.clie_codigo	=	rep.clie_codigo
//			AND	pae.plde_codigo	=	rep.plde_codigo
//			AND	pae.paen_numero	=	rep.paen_numero			
//			AND	IsNull(pae.paen_inspec, 0)	> 0 ;
//					
//		IF sqlca.SQLCode = -1 THEN
//			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Cambio de Altura")
//			
//			RETURN True
////		ELSEIF ii_CantInspecCambio = 0 THEN
////			MessageBox("Atención", "Número de Cambio de Altura no Generará Archivo.")
////			
////			RETURN True
////		ELSEIF ii_CantTarjasCambio > ii_CantInspecCambio THEN
////			MessageBox("Atención", "Se generará un Archivo de Anulación de Inspección.")
////			
////			ib_Anulacion	=	True
////			st_numero.Text	=	"Anulación S.A.G."
////		ELSE
////			MessageBox("Atención", "Se generará un Archivo de Repalletización.")
////			
////			ib_Anulacion	=	False
////			st_numero.Text	=	"Inspección S.A.G."
//		END IF
	END IF

	sle_fechacambio.Text	=	String(id_FechaAltu)
	RETURN False
END IF
end function

on w_info_repalletdeta_sag.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_5=create st_5
this.st_2=create st_2
this.em_numero=create em_numero
this.dw_2=create dw_2
this.st_6=create st_6
this.dw_1=create dw_1
this.cb_buscarepa=create cb_buscarepa
this.st_7=create st_7
this.st_8=create st_8
this.em_repasag=create em_repasag
this.sle_fecha=create sle_fecha
this.em_cambio=create em_cambio
this.st_3=create st_3
this.sle_fechacambio=create sle_fechacambio
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_5
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_numero
this.Control[iCurrent+6]=this.dw_2
this.Control[iCurrent+7]=this.st_6
this.Control[iCurrent+8]=this.dw_1
this.Control[iCurrent+9]=this.cb_buscarepa
this.Control[iCurrent+10]=this.st_7
this.Control[iCurrent+11]=this.st_8
this.Control[iCurrent+12]=this.em_repasag
this.Control[iCurrent+13]=this.sle_fecha
this.Control[iCurrent+14]=this.em_cambio
this.Control[iCurrent+15]=this.st_3
this.Control[iCurrent+16]=this.sle_fechacambio
end on

on w_info_repalletdeta_sag.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.em_numero)
destroy(this.dw_2)
destroy(this.st_6)
destroy(this.dw_1)
destroy(this.cb_buscarepa)
destroy(this.st_7)
destroy(this.st_8)
destroy(this.em_repasag)
destroy(this.sle_fecha)
destroy(this.em_cambio)
destroy(this.st_3)
destroy(this.sle_fechacambio)
end on

event open;call super::open;String	ls_Planta
Integer	li_region

SELECT	plde_nombre, plde_region
	INTO	:ls_Planta,:li_region
	FROM	dba.plantadesp
	WHERE	plde_codigo	=	:gi_CodPlanta ;

dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_CodExport)

dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_1.InsertRow(0)
dw_1.SetItem(1, "plde_codigo", gi_CodPlanta)

istr_mant.argumento[1]	=	String(gi_codplanta)
istr_mant.argumento[2]	=	""
istr_mant.argumento[3]	=	String(gi_codexport)
istr_mant.argumento[4]	=	'0'
istr_mant.argumento[5]	=	ls_Planta					// Planta
istr_mant.argumento[11]	=	String(li_region)
istr_mant.argumento[6]	=	""								// Altura
end event

type st_computador from w_para_informes`st_computador within w_info_repalletdeta_sag
end type

type st_usuario from w_para_informes`st_usuario within w_info_repalletdeta_sag
end type

type st_temporada from w_para_informes`st_temporada within w_info_repalletdeta_sag
end type

type p_logo from w_para_informes`p_logo within w_info_repalletdeta_sag
end type

type st_titulo from w_para_informes`st_titulo within w_info_repalletdeta_sag
integer width = 1902
string text = "Informe de Repalletizado S.A.G."
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_repalletdeta_sag
string tag = "Imprimir Reporte"
integer x = 2432
integer y = 760
integer taborder = 60
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Integer	fila,li_cbx_planta,li_especie,li_numero, li_region
Long		li_planta,li_cliente
String	ls_region

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

IF Long(istr_mant.argumento[4]) = 0 THEN
		MessageBox("Atención", "Repalletizado S.A.G. debe ser Superior a 0.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		em_repasag.SetFocus()

ELSE
		istr_info.titulo	= 'INFORME DE REPALETIZADO S.A.G.'

		OpenWithParm(vinf, istr_info)
		
		vinf.dw_1.DataObject = "dw_info_repalletdeta_sag"
		
		vinf.dw_1.SetTransObject(sqlca)
		
		fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[3]), &
											Integer(istr_mant.Argumento[1]), &
											Long(istr_mant.Argumento[2]), &
											Long(istr_mant.Argumento[6]))
											
		IF fila = -1 THEN
			MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
							"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
		
		ELSEIF fila = 0 THEN
			MessageBox( "No Existe información", "No existe información para este informe.", &
							 StopSign!, Ok!)
		
		ELSE
			F_Membrete(vinf.dw_1)

			
			vinf.dw_1.Modify("frigorifico.text = '" + istr_mant.Argumento[5] + "'")
			vinf.dw_1.Modify("fecha.text = '" + String(Date(istr_mant.Argumento[10])) + "'")
			vinf.dw_1.Modify("repalesag.text = '" + istr_mant.Argumento[4] + "'")	
			vinf.dw_1.Modify("region.text = '" + ls_region + "'")				
			IF gs_Ambiente <> 'Windows' THEN
				F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
			END IF
		END IF
END IF

SetPointer(Arrow!)

end event

type pb_salir from w_para_informes`pb_salir within w_info_repalletdeta_sag
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2432
integer y = 1076
integer taborder = 70
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_repalletdeta_sag
integer x = 251
integer y = 440
integer width = 1902
integer height = 340
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_repalletdeta_sag
integer x = 302
integer y = 632
integer width = 462
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_repalletdeta_sag
integer x = 251
integer y = 780
integer width = 1902
integer height = 340
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_repalletdeta_sag
integer x = 311
integer y = 852
integer width = 517
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "N° Repaletizado"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_numero from editmask within w_info_repalletdeta_sag
integer x = 887
integer y = 840
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

event modified;Integer	li_codexp, li_planta
Long		ll_numero
Date		ld_fzarpe

li_codexp		=	dw_2.Object.clie_codigo[1]
li_planta		=	dw_1.Object.plde_codigo[1]
ll_numero		=	Long(This.Text)
	
SELECT	repe_fecrep
  INTO	:ld_fzarpe
  FROM	dba.REPALLETENCA
 WHERE	clie_codigo	=	:li_codexp
	AND	plde_codigo =	:li_planta
	AND   repe_numero =  :ll_numero;
				
IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla REPALLETENCA")
		em_numero.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Repalletizado Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		em_numero.SetFocus()
	ELSE
		istr_mant.argumento[10]	=	String(ld_fzarpe)
		istr_mant.argumento[2]	=	This.Text		
		sle_fecha.text				= 	String(ld_fzarpe)
		em_cambio.SetFocus()
		//em_repasag.SetFocus()
	END IF

end event

type dw_2 from datawindow within w_info_repalletdeta_sag
integer x = 887
integer y = 500
integer width = 1230
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[3]	=	data
ELSE
	This.SetItem(1, "clie_codigo", gi_CodExport)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_repalletdeta_sag
integer x = 302
integer y = 508
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_info_repalletdeta_sag
integer x = 887
integer y = 620
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Cliente

li_Cliente	=	dw_2.Object.clie_codigo[1]

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	istr_mant.Argumento[5]	=	ls_Columna[1]
	istr_mant.Argumento[11]	=	ls_Columna[11]
ELSE
	This.SetItem(1, "plde_codigo", gi_CodPlanta)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type cb_buscarepa from commandbutton within w_info_repalletdeta_sag
integer x = 1303
integer y = 844
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

event clicked;istr_busq.argum[1]	=	String(dw_2.Object.clie_codigo[1])
istr_busq.argum[2]	=	String(dw_1.Object.plde_codigo[1])
istr_busq.argum[3]	=	'1'

OpenWithParm(w_busc_repalletenca, istr_busq)

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

type st_7 from statictext within w_info_repalletdeta_sag
integer x = 251
integer y = 1124
integer width = 1902
integer height = 212
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_repalletdeta_sag
integer x = 311
integer y = 1192
integer width = 549
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Correlativo S.A.G."
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_repasag from editmask within w_info_repalletdeta_sag
integer x = 887
integer y = 1180
integer width = 393
integer height = 92
integer taborder = 50
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

event modified;istr_mant.argumento[4]	=	This.Text

IF Long(This.Text) = 0 THEN
		MessageBox("Atención", "Repalletizado S.A.G. debe ser Superior a 0.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		em_repasag.SetFocus()
	END IF
end event

type sle_fecha from singlelineedit within w_info_repalletdeta_sag
integer x = 1417
integer y = 844
integer width = 434
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

type em_cambio from editmask within w_info_repalletdeta_sag
event getfocus pbm_ensetfocus
integer x = 891
integer y = 976
integer width = 393
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
string displaydata = "$"
end type

event modified;IF This.Text <> "" THEN
	IF NoExisteFolioAltura(Long(This.Text)) THEN
		This.Text	=	""
		
		This.SetFocus()
	ELSE
		istr_mant.argumento[6]	=	String(Long(This.Text), '00000')
		em_repasag.SetFocus()
	END IF
END IF
end event

type st_3 from statictext within w_info_repalletdeta_sag
integer x = 311
integer y = 976
integer width = 549
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
boolean enabled = false
string text = "Nro.Cambio Altura"
boolean focusrectangle = false
end type

type sle_fechacambio from singlelineedit within w_info_repalletdeta_sag
integer x = 1417
integer y = 976
integer width = 434
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

