$PBExportHeader$w_genera_registro_despacho.srw
$PBExportComments$Ventana de Generacion de Registro de Despacho Inter Planta.
forward
global type w_genera_registro_despacho from window
end type
type dw_6 from datawindow within w_genera_registro_despacho
end type
type em_fecha from editmask within w_genera_registro_despacho
end type
type st_2 from statictext within w_genera_registro_despacho
end type
type cb_3 from commandbutton within w_genera_registro_despacho
end type
type em_nro_despacho from editmask within w_genera_registro_despacho
end type
type st_4 from statictext within w_genera_registro_despacho
end type
type dw_planta from datawindow within w_genera_registro_despacho
end type
type st_3 from statictext within w_genera_registro_despacho
end type
type dw_5 from datawindow within w_genera_registro_despacho
end type
type dw_4 from datawindow within w_genera_registro_despacho
end type
type dw_3 from datawindow within w_genera_registro_despacho
end type
type sle_mensa from singlelineedit within w_genera_registro_despacho
end type
type st_5 from statictext within w_genera_registro_despacho
end type
type dw_2 from datawindow within w_genera_registro_despacho
end type
type st_1 from statictext within w_genera_registro_despacho
end type
type pb_salir from picturebutton within w_genera_registro_despacho
end type
type pb_grabar from picturebutton within w_genera_registro_despacho
end type
type gb_2 from groupbox within w_genera_registro_despacho
end type
type gb_1 from groupbox within w_genera_registro_despacho
end type
type st_6 from statictext within w_genera_registro_despacho
end type
type dw_1 from datawindow within w_genera_registro_despacho
end type
end forward

global type w_genera_registro_despacho from window
integer width = 2441
integer height = 1056
boolean titlebar = true
string title = "Genera Archivo Despacho Inter Planta"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
string icon = "TABLA.ICO"
event ue_guardar ( )
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
event ue_genera_facturas pbm_custom04
dw_6 dw_6
em_fecha em_fecha
st_2 st_2
cb_3 cb_3
em_nro_despacho em_nro_despacho
st_4 st_4
dw_planta dw_planta
st_3 st_3
dw_5 dw_5
dw_4 dw_4
dw_3 dw_3
sle_mensa sle_mensa
st_5 st_5
dw_2 dw_2
st_1 st_1
pb_salir pb_salir
pb_grabar pb_grabar
gb_2 gb_2
gb_1 gb_1
st_6 st_6
dw_1 dw_1
end type
global w_genera_registro_despacho w_genera_registro_despacho

type variables
Date							id_FechaAcceso
Time							it_HoraAcceso

str_mant						istr_mant

DataWindowChild			idwc_planta

uo_plantadesp 	 			iuo_plantadesp
uo_movtofrutacomenca		iuo_movtofrutacomenca
end variables

forward prototypes
public function boolean existeplanilla (long al_planilla)
end prototypes

event ue_guardar();Long			ll_Fila, ll_Filas, ll_FilaP, ll_FilaDet, ll_Numero, ll_Lote
Integer		li_PldSag, li_PlantaLote, li_EspecieLote, li_TipoMovtoEnv
String		ls_Archivo, ls_Registro

dw_1.Reset()
dw_2.Reset()
dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()

dw_1.SetTransObject(SqlCa)	// Registro
dw_2.SetTransObject(SqlCa)	//	Encabezado Despacho Fruta Comercial
dw_3.SetTransObject(SqlCa)	//	Detalle Despacho Fruta Comercial
dw_4.SetTransObject(SqlCa)	//	Encabezado Lote Fruta Comercial
dw_5.SetTransObject(SqlCa)	//	Detalle Lote Fruta Comercial
dw_6.SetTransObject(SqlCa)	//	Detalle Movto. Envase

li_TipoMovtoEnv	=	62		// Movto. de Envase asociado a Fruta Comercial

ll_Numero			=	Long(em_nro_despacho.Text)

ll_Filas		= dw_2.Retrieve(Integer(istr_Mant.Argumento[1]), &
									 Integer(istr_Mant.Argumento[2]), ll_Numero)

IF ll_Filas = -1 THEN
	F_ErrorBaseDatos(sqlca,"Recuperación datos de Despacho.")
ELSEIF ll_Filas = 0 THEN
	MessageBox("Atención", "No hay información para Despacho Indicado.~r~rIngrese otro Número.", &
					Exclamation!, Ok!)
	pb_grabar.Enabled	= False
	em_nro_despacho.SetFocus()
ELSE
	//	Encabezado Despacho Fruta Comercial
	FOR ll_Fila = 1 TO ll_Filas
		ls_Registro =	"1"

		ls_Registro +=	String(Integer(istr_Mant.Argumento[1]),'0000')
		ls_Registro +=	String(Integer(istr_Mant.Argumento[2]),'000')
		IF IsNull(dw_2.Object.mfco_fecmov[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',8)
		ELSE	
			ls_Registro +=	String(Date(dw_2.Object.mfco_fecmov[ll_Fila]),'DDMMYYYY')
		END IF	
		IF IsNull(dw_2.Object.moti_codigo[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',2)
		ELSE	
			ls_Registro +=	String(dw_2.Object.moti_codigo[ll_Fila],'00')
		END IF
		IF IsNull(dw_2.Object.prod_codigo[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro +=	String(dw_2.Object.prod_codigo[ll_Fila],'0000')
		END IF
		IF IsNull(dw_2.Object.plde_coorde[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro +=	String(dw_2.Object.plde_coorde[ll_Fila],'0000')
		END IF
		IF IsNull(dw_2.Object.tran_codigo[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro +=	String(dw_2.Object.tran_codigo[ll_Fila],'0000')
		END IF
		IF IsNull(dw_2.Object.clpr_rut[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',10)
		ELSE	
			ls_Registro	+=	dw_2.Object.clpr_rut[ll_Fila] + &
								Fill(' ',(10)-Len(String(dw_2.Object.clpr_rut[ll_Fila])))
		END IF
		IF IsNull(dw_2.Object.cami_clasifi[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',1)
		ELSE	
			ls_Registro +=	String(dw_2.Object.cami_clasifi[ll_Fila],'0')
		END IF
		IF IsNull(dw_2.Object.cami_patent[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',10)
		ELSE	
			ls_Registro	+=	dw_2.Object.cami_patent[ll_Fila] + &
								Fill(' ',(10)-Len(String(dw_2.Object.cami_patent[ll_Fila])))
		END IF
		IF IsNull(dw_2.Object.cami_patcar[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',10)
		ELSE
			ls_Registro	+=	dw_2.Object.cami_patcar[ll_Fila] + &
								Fill(' ',(10)-Len(String(dw_2.Object.cami_patcar[ll_Fila])))
		END IF
		IF IsNull(dw_2.Object.mfco_guisii[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',8)
		ELSE	
			ls_Registro +=	String(dw_2.Object.mfco_guisii[ll_Fila],'00000000')
		END IF
		IF IsNull(dw_2.Object.mfco_rutcho[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',10)
		ELSE	
			ls_Registro	+=	dw_2.Object.mfco_rutcho[ll_Fila] + &
								Fill(' ',(10)-Len(String(dw_2.Object.mfco_rutcho[ll_Fila])))
		END IF
		IF IsNull(dw_2.Object.mfco_chofer[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',40)
		ELSE	
			ls_Registro	+=	dw_2.Object.mfco_chofer[ll_Fila] + &
								Fill(' ',(40)-Len(String(dw_2.Object.mfco_chofer[ll_Fila])))
		END IF
		IF IsNull(dw_2.Object.mfco_totbul[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro +=	String(dw_2.Object.mfco_totbul[ll_Fila],'0000')
		END IF
		IF IsNull(dw_2.Object.mfco_tpneto[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',12)
		ELSE
			ls_Registro +=	String(dw_2.Object.mfco_tpneto[ll_Fila],'00000000.000')
		END IF

		ll_FilaDet	=	dw_1.InsertRow(0)
		dw_1.Object.Registro[ll_FilaDet]	=	ls_Registro
	NEXT

	// Detalle Despacho Fruta Comercial
	ll_Filas	=	dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), &
									  Integer(istr_Mant.Argumento[2]), &
									  ll_Numero)

	FOR ll_Fila = 1 TO ll_Filas
		ls_Registro =	"2"
		IF IsNull(dw_3.Object.plde_coorde[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro +=	String(dw_3.Object.plde_coorde[ll_Fila],'0000')
		END IF
		IF IsNull(dw_3.Object.cama_codigo[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro +=	String(dw_3.Object.cama_codigo[ll_Fila],'0000')
		END IF
		IF IsNull(dw_3.Object.lofc_pltcod[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro +=	String(dw_3.Object.lofc_pltcod[ll_Fila],'0000')
		END IF
		IF IsNull(dw_3.Object.lofc_espcod[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',2)
		ELSE	
			ls_Registro +=	String(dw_3.Object.lofc_espcod[ll_Fila],'00')
		END IF
		IF IsNull(dw_3.Object.lofc_lotefc[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',8)
		ELSE	
			ls_Registro +=	String(dw_3.Object.lofc_lotefc[ll_Fila],'00000000')
		END IF
		IF IsNull(dw_3.Object.lfcd_secuen[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',2)
		ELSE	
			ls_Registro +=	String(dw_3.Object.lfcd_secuen[ll_Fila],'00')
		END IF
		IF IsNull(dw_3.Object.mfcd_bulent[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',9)
		ELSE	
			ls_Registro +=	String(dw_3.Object.mfcd_bulent[ll_Fila],'000000.00')
		END IF
		IF IsNull(dw_3.Object.mfcd_kgnent[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',12)
		ELSE	
			ls_Registro +=	String(dw_3.Object.mfcd_kgnent[ll_Fila],'00000000.000')
		END IF
		IF IsNull(dw_3.Object.mfcd_kilrom[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',12)
		ELSE	
			ls_Registro +=	String(dw_3.Object.mfcd_kilrom[ll_Fila],'00000000.000')
		END IF

		ll_FilaDet	=	dw_1.InsertRow(0)
		dw_1.Object.Registro[ll_FilaDet]	=	ls_Registro
	NEXT

	FOR ll_Fila = 1 TO ll_Filas
		li_PlantaLote	=	dw_3.Object.lofc_pltcod[ll_Fila]
		li_EspecieLote	=	dw_3.Object.lofc_espcod[ll_Fila]
		ll_Lote			=	dw_3.Object.lofc_lotefc[ll_Fila]
		
		//	Encabezado Lote Fruta Comercial
		ll_FilaP	=	dw_4.Retrieve(li_PlantaLote, li_EspecieLote, ll_Lote)

		IF ll_FilaP > 0  THEN
			ls_Registro =	"3"
			IF IsNull(dw_4.Object.prod_codigo[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',4)
			ELSE
				ls_Registro +=	String(dw_4.Object.prod_codigo[ll_FilaP],'0000')
			END IF
			IF IsNull(dw_4.Object.cate_codigo[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',3)
			ELSE
				ls_Registro +=	String(dw_4.Object.cate_codigo[ll_FilaP],'000')
			END IF
			IF IsNull(dw_4.Object.sepl_codigo[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',2)
			ELSE
				ls_Registro +=	String(dw_4.Object.sepl_codigo[ll_FilaP],'00')
			END IF
			IF IsNull(dw_4.Object.lofc_diagra[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',3)
			ELSE
				ls_Registro +=	String(dw_4.Object.lofc_diagra[ll_FilaP],'000')
			END IF
			IF IsNull(dw_4.Object.lofc_grucal[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',3)
			ELSE
				ls_Registro	+=	dw_4.Object.lofc_grucal[ll_FilaP] + &
									Fill(' ',(3)-Len(String(dw_4.Object.lofc_grucal[ll_FilaP])))
			END IF
			IF IsNull(dw_4.Object.lofc_totbul[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',4)
			ELSE	
				ls_Registro +=	String(dw_4.Object.lofc_totbul[ll_FilaP],'0000')
			END IF
			IF IsNull(dw_4.Object.lofc_totkil[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',12)
			ELSE	
				ls_Registro +=	String(dw_4.Object.lofc_totkil[ll_FilaP],'00000000.000')
			END IF

			ll_FilaDet	=	dw_1.InsertRow(0)
			dw_1.Object.Registro[ll_FilaDet]	=	ls_Registro
		END IF

		//	Detalle Lote Fruta Comercial
		ll_FilaP	=	dw_5.Retrieve(li_PlantaLote, li_EspecieLote, ll_Lote)

		IF ll_FilaP > 0  THEN
			ls_Registro =	"4"

			IF IsNull(dw_5.Object.lfcd_secuen[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',2)
			ELSE
				ls_Registro +=	String(dw_5.Object.lfcd_secuen[ll_FilaP],'00')
			END IF
			IF IsNull(dw_5.Object.lfcd_nturno[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',1)
			ELSE
				ls_Registro +=	String(dw_5.Object.lfcd_nturno[ll_FilaP],'0')
			END IF
			IF IsNull(dw_5.Object.lfcd_fecham[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',8)
			ELSE
				ls_Registro	+=	String(dw_5.Object.lfcd_fecham[ll_FilaP], 'DDMMYYYY')
			END IF
			IF IsNull(dw_5.Object.prod_codigo[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',4)
			ELSE
				ls_Registro +=	String(dw_5.Object.prod_codigo[ll_FilaP],'0000')
			END IF
			IF IsNull(dw_5.Object.vari_codigo[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',4)
			ELSE
				ls_Registro +=	String(dw_5.Object.vari_codigo[ll_FilaP],'0000')
			END IF
			IF IsNull(dw_5.Object.frio_tipofr[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',2)
			ELSE
				ls_Registro	+=	dw_5.Object.frio_tipofr[ll_FilaP] + &
									Fill(' ',(2)-Len(String(dw_5.Object.frio_tipofr[ll_FilaP])))
			END IF
			IF IsNull(dw_5.Object.pefr_codigo[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',2)
			ELSE
				ls_Registro +=	String(dw_5.Object.pefr_codigo[ll_FilaP],'00')
			END IF
			IF IsNull(dw_5.Object.enva_tipoen[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',1)
			ELSE
				ls_Registro +=	String(dw_5.Object.enva_tipoen[ll_FilaP],'0')
			END IF
			IF IsNull(dw_5.Object.enva_codigo[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',3)
			ELSE
				ls_Registro +=	String(dw_5.Object.enva_tipoen[ll_FilaP],'000')
			END IF
			IF IsNull(dw_5.Object.refe_calibr[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',4)
			ELSE
				ls_Registro	+=	dw_5.Object.refe_calibr[ll_FilaP] + &
									Fill(' ',(4)-Len(String(dw_5.Object.refe_calibr[ll_FilaP])))
			END IF
			IF IsNull(dw_5.Object.refe_gcalib[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',4)
			ELSE
				ls_Registro	+=	dw_5.Object.refe_gcalib[ll_FilaP] + &
									Fill(' ',(4)-Len(String(dw_5.Object.refe_gcalib[ll_FilaP])))
			END IF
			IF IsNull(dw_5.Object.lfcd_bultos[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',9)
			ELSE	
				ls_Registro +=	String(dw_5.Object.lfcd_bultos[ll_FilaP],'000000.00')
			END IF
			IF IsNull(dw_5.Object.lfcd_kilnet[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',12)
			ELSE	
				ls_Registro +=	String(dw_5.Object.lfcd_kilnet[ll_FilaP],'00000000.000')
			END IF
			IF IsNull(dw_5.Object.lfcd_kilpro[ll_FilaP]) THEN
				ls_Registro	+= Fill(' ',12)
			ELSE	
				ls_Registro +=	String(dw_5.Object.lfcd_kilpro[ll_FilaP],'00000000.000')
			END IF

			ll_FilaDet	=	dw_1.InsertRow(0)
			dw_1.Object.Registro[ll_FilaDet]	=	ls_Registro

		END IF
	NEXT

	// Detale Movto. Envase Fruta Comercial
	ll_Filas	=	dw_6.Retrieve(Integer(istr_Mant.Argumento[1]), &
									  li_TipoMovtoEnv, ll_Numero, 2)

	FOR ll_Fila = 1 TO ll_Filas
		ls_Registro =	"5"
		
		IF IsNull(dw_6.Object.enva_tipoen[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',1)
		ELSE	
			ls_Registro	+=	String(dw_6.Object.enva_tipoen[ll_Fila], '0')
		END IF 
		
		IF IsNull(dw_6.Object.enva_codigo[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',3)
		ELSE	
			ls_Registro	+=	String(dw_6.Object.enva_codigo[ll_Fila], '000')
		END IF
	
		IF IsNull(dw_6.Object.fgme_conenv[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',1)
		ELSE	
			ls_Registro	+=	String(dw_6.Object.fgme_conenv[ll_Fila], '0')
		END IF
	
		IF IsNull(dw_6.Object.cale_calida[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro	+=	String(dw_6.Object.cale_calida[ll_Fila])
		END IF
	
		IF IsNull(dw_6.Object.fgme_sentid[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',1)
		ELSE	
			ls_Registro	+=	String(dw_6.Object.fgme_sentid[ll_Fila], '0')
		END IF
	
		IF IsNull(dw_6.Object.fgme_cantid[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro	+=	String(dw_6.Object.fgme_cantid[ll_Fila], '0000')
		END IF
	
		IF IsNull(dw_6.Object.fgme_pesone[ll_Fila]) THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro	+=	String(dw_6.Object.fgme_pesone[ll_fila], '0000.000')
		END IF
	
		ll_FilaDet	=	dw_1.InsertRow(0)
		dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
		
	NEXT

	ls_Registro	= '&&'

	ll_FilaDet	=	dw_1.InsertRow(0)
	dw_1.Object.Registro[ll_FilaDet]	=	ls_Registro
	
	ls_Archivo	= String(ll_Numero,'00000000') + ".FCM"

	IF dw_1.SaveAs("C:\Generados\" + ls_Archivo, Text!, False) = -1 THEN
		MessageBox("Atención","No se pudo generar el archivo " + ls_Archivo)
	ELSE
		sle_mensa.text	= "Archivo " + ls_Archivo + " Generado. Avise a Computación"
	END IF
	
	dw_2.Reset()

END IF

em_nro_despacho.SetFocus()
end event

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

public function boolean existeplanilla (long al_planilla);Integer	li_codexp, li_planta
Date  	ld_fecha

li_codexp		=	Integer(istr_mant.argumento[1])
li_planta		=	Integer(istr_mant.argumento[2])

IF (al_planilla <> 0) OR li_planta = 0 THEN
	SELECT Min(mfee_fecmov)
		INTO	:ld_fecha
		FROM	dba.spro_movtofrutaembaenca
		WHERE	plde_codigo =	:li_planta
		AND	expo_codigo	=	:li_codexp
		AND	mfee_plasag	=	:al_planilla ;
				
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_movtofrutaembaenca")
		em_nro_despacho.SetFocus()
		RETURN False
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_grabar.Enabled	= False
		em_nro_despacho.SetFocus()
		RETURN False
	ELSEIF IsNull(ld_fecha) THEN
		MessageBox("Atención", "No existe Planilla S.A.G. Indicado.~r~rIngrese otro Número.", &
						Exclamation!, Ok!)
		pb_grabar.Enabled	= False
		em_nro_despacho.SetFocus()
		RETURN False
	ELSE
		em_fecha.text		= String(Date(ld_fecha))
		sle_mensa.text		= ""
		pb_grabar.Enabled	= True
		RETURN True
	END IF
ELSE
	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
					Exclamation!, Ok!)
	RETURN False
END IF
end function

on w_genera_registro_despacho.create
this.dw_6=create dw_6
this.em_fecha=create em_fecha
this.st_2=create st_2
this.cb_3=create cb_3
this.em_nro_despacho=create em_nro_despacho
this.st_4=create st_4
this.dw_planta=create dw_planta
this.st_3=create st_3
this.dw_5=create dw_5
this.dw_4=create dw_4
this.dw_3=create dw_3
this.sle_mensa=create sle_mensa
this.st_5=create st_5
this.dw_2=create dw_2
this.st_1=create st_1
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.gb_2=create gb_2
this.gb_1=create gb_1
this.st_6=create st_6
this.dw_1=create dw_1
this.Control[]={this.dw_6,&
this.em_fecha,&
this.st_2,&
this.cb_3,&
this.em_nro_despacho,&
this.st_4,&
this.dw_planta,&
this.st_3,&
this.dw_5,&
this.dw_4,&
this.dw_3,&
this.sle_mensa,&
this.st_5,&
this.dw_2,&
this.st_1,&
this.pb_salir,&
this.pb_grabar,&
this.gb_2,&
this.gb_1,&
this.st_6,&
this.dw_1}
end on

on w_genera_registro_despacho.destroy
destroy(this.dw_6)
destroy(this.em_fecha)
destroy(this.st_2)
destroy(this.cb_3)
destroy(this.em_nro_despacho)
destroy(this.st_4)
destroy(this.dw_planta)
destroy(this.st_3)
destroy(this.dw_5)
destroy(this.dw_4)
destroy(this.dw_3)
destroy(this.sle_mensa)
destroy(this.st_5)
destroy(this.dw_2)
destroy(this.st_1)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.gb_2)
destroy(this.gb_1)
destroy(this.st_6)
destroy(this.dw_1)
end on

event open;/*
Argumentos :	[1]	=>	Código de Planta
					[2]	=>	Tipo de Movimiento
					[3]	=>	Número de Movimiento
*/

x	=	0
y	=	0

This.Icon	=	Gstr_apl.Icono

iuo_movtofrutacomenca	=	Create uo_movtofrutacomenca
em_nro_despacho.setfocus()

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.Retrieve()
dw_planta.InsertRow(0)
dw_planta.SetItem(1,"plde_codigo",gstr_paramplanta.CodigoPlanta)

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	"22"

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type dw_6 from datawindow within w_genera_registro_despacho
boolean visible = false
integer x = 37
integer y = 1764
integer width = 2181
integer height = 112
integer taborder = 50
string dataobject = "dw_mues_movtoenvadeta"
boolean maxbox = true
boolean livescroll = true
end type

event clicked;This.Print()
end event

type em_fecha from editmask within w_genera_registro_despacho
integer x = 1527
integer y = 400
integer width = 402
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_2 from statictext within w_genera_registro_despacho
integer x = 1303
integer y = 412
integer width = 197
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha"
boolean focusrectangle = false
end type

type cb_3 from commandbutton within w_genera_registro_despacho
integer x = 1134
integer y = 404
integer width = 96
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;Str_Busqueda	lstr_busq

lstr_Busq.Argum[1]	=	String(dw_planta.Object.plde_codigo[1])
lstr_Busq.Argum[2]	=	istr_Mant.Argumento[2]
lstr_Busq.Argum[3]	=	""
lstr_Busq.Argum[4]	=	""

OpenWithParm(w_busc_movtofrutacomenca, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	em_nro_despacho.text	=	lstr_Busq.Argum[3]
	em_fecha.text 			=	Mid(lstr_Busq.Argum[4],1,10)
	pb_grabar.Enabled	= True
END IF
end event

type em_nro_despacho from editmask within w_genera_registro_despacho
integer x = 777
integer y = 400
integer width = 343
integer height = 92
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;IF iuo_movtofrutacomenca.Existe(Integer(istr_Mant.Argumento[1]), &
										  Integer(istr_Mant.Argumento[2]), &
										  Long(This.Text),True,SqlCa) THEN
	em_fecha.Text	=	String(Date(iuo_movtofrutacomenca.FechaMovto))
	istr_Mant.Argumento[3]	=	This.Text
	pb_grabar.Enabled	= True
ELSE
	em_fecha.Text	=	''
	This.SetFocus()
	RETURN 1
END IF
end event

type st_4 from statictext within w_genera_registro_despacho
integer x = 169
integer y = 412
integer width = 544
integer height = 68
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Número Despacho"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_genera_registro_despacho
string tag = "Planta"
integer x = 777
integer y = 292
integer width = 869
integer height = 92
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_null

IF iuo_plantadesp.existe(Integer(data),True,SQLCa) = True THEN
	istr_Mant.Argumento[1] = Data
ELSE
	dw_planta.SetItem(1,"plde_codigo",SetNull(li_null))
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_3 from statictext within w_genera_registro_despacho
integer x = 169
integer y = 304
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type dw_5 from datawindow within w_genera_registro_despacho
boolean visible = false
integer x = 37
integer y = 1648
integer width = 2181
integer height = 112
string dataobject = "dw_gene_spro_lotesfrutacomdeta"
boolean maxbox = true
boolean livescroll = true
end type

event clicked;This.Print()
end event

type dw_4 from datawindow within w_genera_registro_despacho
boolean visible = false
integer x = 37
integer y = 1532
integer width = 2181
integer height = 112
string dataobject = "dw_gene_spro_lotesfrutacomenc"
boolean maxbox = true
boolean livescroll = true
end type

event clicked;This.Print()
end event

type dw_3 from datawindow within w_genera_registro_despacho
boolean visible = false
integer x = 37
integer y = 1416
integer width = 2181
integer height = 112
string dataobject = "dw_mues_movtofrutacomdeta"
boolean maxbox = true
boolean livescroll = true
end type

event clicked;This.Print()
end event

type sle_mensa from singlelineedit within w_genera_registro_despacho
integer x = 238
integer y = 728
integer width = 1632
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 16777215
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_genera_registro_despacho
integer x = 82
integer y = 68
integer width = 1938
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
string text = "Generación de Archivo Despacho Inter Planta"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_genera_registro_despacho
boolean visible = false
integer x = 37
integer y = 1300
integer width = 2181
integer height = 112
string dataobject = "dw_mant_movtofrutacomenca"
boolean maxbox = true
boolean livescroll = true
end type

event clicked;This.Print()
end event

type st_1 from statictext within w_genera_registro_despacho
integer x = 82
integer y = 180
integer width = 1938
integer height = 480
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_genera_registro_despacho
integer x = 2089
integer y = 540
integer width = 300
integer height = 245
integer taborder = 40
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_genera_registro_despacho
integer x = 2089
integer y = 284
integer width = 300
integer height = 245
integer taborder = 30
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_guardar")
end event

type gb_2 from groupbox within w_genera_registro_despacho
boolean visible = false
integer x = 2501
integer y = 228
integer width = 274
integer height = 268
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
end type

type gb_1 from groupbox within w_genera_registro_despacho
boolean visible = false
integer x = 2501
integer y = 488
integer width = 274
integer height = 268
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
end type

type st_6 from statictext within w_genera_registro_despacho
integer x = 82
integer y = 660
integer width = 1938
integer height = 224
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_genera_registro_despacho
boolean visible = false
integer x = 37
integer y = 1100
integer width = 2181
integer height = 168
boolean bringtotop = true
string dataobject = "dw_gene_archivo_plano"
boolean livescroll = true
end type

