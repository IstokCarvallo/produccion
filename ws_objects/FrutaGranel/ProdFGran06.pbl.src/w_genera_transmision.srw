$PBExportHeader$w_genera_transmision.srw
forward
global type w_genera_transmision from window
end type
type rb_despa from radiobutton within w_genera_transmision
end type
type rb_prefac from radiobutton within w_genera_transmision
end type
type rb_existencia from radiobutton within w_genera_transmision
end type
type rb_traspaso from radiobutton within w_genera_transmision
end type
type dw_lotesfgdeta from datawindow within w_genera_transmision
end type
type dw_movtoenvaenca from datawindow within w_genera_transmision
end type
type cb_3 from commandbutton within w_genera_transmision
end type
type st_5 from statictext within w_genera_transmision
end type
type em_fecha from editmask within w_genera_transmision
end type
type dw_dyfre from datawindow within w_genera_transmision
end type
type dw_colorfondorec from datawindow within w_genera_transmision
end type
type dw_distcalibre from datawindow within w_genera_transmision
end type
type dw_colcubcatre from datawindow within w_genera_transmision
end type
type dw_madcoschare from datawindow within w_genera_transmision
end type
type dw_movtoenvadeta from datawindow within w_genera_transmision
end type
type dw_lotesfgenca from datawindow within w_genera_transmision
end type
type dw_movtofgenca from datawindow within w_genera_transmision
end type
type dw_movtofgdeta from datawindow within w_genera_transmision
end type
type dw_planta from datawindow within w_genera_transmision
end type
type pb_grabar from picturebutton within w_genera_transmision
end type
type sle_mensaje from singlelineedit within w_genera_transmision
end type
type st_6 from statictext within w_genera_transmision
end type
type cb_1 from commandbutton within w_genera_transmision
end type
type em_tipomovto from editmask within w_genera_transmision
end type
type em_nro_despacho from editmask within w_genera_transmision
end type
type st_4 from statictext within w_genera_transmision
end type
type st_3 from statictext within w_genera_transmision
end type
type st_2 from statictext within w_genera_transmision
end type
type st_1 from statictext within w_genera_transmision
end type
type dw_registro from datawindow within w_genera_transmision
end type
type gb_1 from groupbox within w_genera_transmision
end type
type pb_salir from picturebutton within w_genera_transmision
end type
type gb_2 from groupbox within w_genera_transmision
end type
type gb_3 from groupbox within w_genera_transmision
end type
type st_7 from statictext within w_genera_transmision
end type
end forward

global type w_genera_transmision from window
integer x = 521
integer y = 656
integer width = 3415
integer height = 1500
boolean titlebar = true
string title = "Genera Archivo Despacho Inter Planta"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 12632256
event ue_guardar ( ) External
rb_despa rb_despa
rb_prefac rb_prefac
rb_existencia rb_existencia
rb_traspaso rb_traspaso
dw_lotesfgdeta dw_lotesfgdeta
dw_movtoenvaenca dw_movtoenvaenca
cb_3 cb_3
st_5 st_5
em_fecha em_fecha
dw_dyfre dw_dyfre
dw_colorfondorec dw_colorfondorec
dw_distcalibre dw_distcalibre
dw_colcubcatre dw_colcubcatre
dw_madcoschare dw_madcoschare
dw_movtoenvadeta dw_movtoenvadeta
dw_lotesfgenca dw_lotesfgenca
dw_movtofgenca dw_movtofgenca
dw_movtofgdeta dw_movtofgdeta
dw_planta dw_planta
pb_grabar pb_grabar
sle_mensaje sle_mensaje
st_6 st_6
cb_1 cb_1
em_tipomovto em_tipomovto
em_nro_despacho em_nro_despacho
st_4 st_4
st_3 st_3
st_2 st_2
st_1 st_1
dw_registro dw_registro
gb_1 gb_1
pb_salir pb_salir
gb_2 gb_2
gb_3 gb_3
st_7 st_7
end type
global w_genera_transmision w_genera_transmision

type variables
str_mant               istr_mant

Integer ii_planta

DataWindowChild idwc_planta,	idwc_madurez, idwc_defectos, idwc_colorfondo, &
					 idwc_camara
					 
uo_plantadesp 	 iuo_plantadesp

end variables

forward prototypes
public subroutine genera_controlcalidad (integer ai_planta, integer ai_tipo, long al_nrodesp, integer ai_plantades)
public subroutine genera_traspaso ()
public subroutine existemovimiento (integer ai_planta, integer ai_tipomov, long al_numero)
end prototypes

event ue_guardar();IF rb_traspaso.Checked THEN
	genera_traspaso()
END IF	
end event

public subroutine genera_controlcalidad (integer ai_planta, integer ai_tipo, long al_nrodesp, integer ai_plantades);String   ls_registro
Integer li_planta, li_especie, li_variedad, li_cosecha, li_color, &
        li_Filadet,  li_calibre, li_dadefe, li_colorfon
Long    ll_filagn, ll_fila, ll_lote, ll_cantlotes


//CONTROL DE CALIDAD
dw_movtofgenca.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.Retrieve(gi_codexport)

ll_cantlotes	=	dw_lotesfgenca.retrieve(ai_planta,ai_tipo,al_nrodesp)

FOR ll_filagn =	1	TO ll_cantlotes
	li_planta	=	dw_lotesfgenca.Object.lote_pltcod[ll_filagn]
	li_especie	=	dw_lotesfgenca.Object.lote_espcod[ll_filagn]
	ll_lote		=	dw_lotesfgenca.Object.lote_codigo[ll_filagn]
	li_variedad	=	dw_lotesfgenca.Object.vari_codigo[ll_filagn]
	
	//MADUREZ DE COSECHA
	dw_madcoschare.GetChild("cofo_codigo", idwc_madurez)
	idwc_madurez.SetTransObject(SQLCA)
	idwc_madurez.Retrieve(li_especie, li_variedad)
	dw_madcoschare.InsertRow(0)

	IF dw_madcoschare.Retrieve(li_planta,li_especie,ll_lote,li_variedad) > 0 THEN
		li_cosecha	=	dw_madcoschare.Retrieve(li_planta,li_especie,ll_lote,li_variedad)
		FOR ll_fila = 1 TO li_cosecha	
			ls_Registro =	"6"
	
			IF ISNULL(dw_madcoschare.Object.lote_pltcod[ll_fila])THEN
				ls_Registro	+= Fill(' ',4)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.lote_pltcod[ll_fila], '0000')
			END IF
	
			IF ISNULL(dw_madcoschare.Object.lote_espcod[ll_fila])THEN
				ls_Registro	+= Fill(' ',2)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.lote_espcod[ll_fila], '00')
			END IF
	
			IF ISNULL(dw_madcoschare.Object.lote_codigo[ll_fila])THEN
				ls_Registro	+= Fill(' ',4)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.lote_codigo[ll_fila], '0000')
			END IF 
	
			IF ISNULL(dw_madcoschare.Object.mcor_grucal[ll_fila])THEN
				ls_Registro	+= Fill(' ',3)
			ELSE	
				ls_Registro	+=	dw_madcoschare.Object.mcor_grucal[ll_fila] + &
									Fill(' ',3-Len(String(dw_madcoschare.Object.mcor_grucal[ll_fila])))
			END IF 
	
			IF ISNULL(dw_madcoschare.Object.vari_codigo[ll_fila])THEN
				ls_Registro	+= Fill(' ',4)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.vari_codigo[ll_fila], '0000')
			END IF 
	
			IF ISNULL(dw_madcoschare.Object.mcor_secuen[ll_fila])THEN
				ls_Registro	+= Fill(' ',2)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_secuen[ll_fila], '00')
			END IF 
	
			IF ISNULL(dw_madcoschare.Object.cofo_codigo[ll_fila])THEN
				ls_Registro	+= Fill(' ',2)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.cofo_codigo[ll_fila], '00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_ssolpr[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_ssolpr[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_ssolmi[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_ssolmi[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_ssolma[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_ssolma[ll_fila], '00.00')
			END IF
			
			IF ISNULL(dw_madcoschare.Object.mcor_preepr[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_preepr[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_preemi[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_preemi[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_preema[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_preema[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_prehpr[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_prehpr[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_prehmi[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_prehmi[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_prehma[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_prehma[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_preapr[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_preapr[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_preami[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_preami[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_preama[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_preama[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_talmpr[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_talmpr[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_talmmi[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_talmmi[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_talmma[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_talmma[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_coacle[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_coacle[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_coacmo[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_coacmo[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_coacse[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_coacse[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_comole[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_comole[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_comomo[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_comomo[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_comose[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_comose[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_acidpr[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_acidpr[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_acidmi[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_acidmi[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_acidma[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_acidma[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_harile[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_harile[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_harimo[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_harimo[ll_fila], '00.00')
			END IF
			
			IF ISNULL(dw_madcoschare.Object.mcor_harise[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_harise[ll_fila], '00.00')
			END IF
			
			IF ISNULL(dw_madcoschare.Object.mcor_grasse[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_grasse[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_grasmo[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_grasmo[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_grasle[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_grasle[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_sofipr[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_sofipr[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_sofimi[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_sofimi[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_sofima[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_sofima[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_matspr[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_matspr[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_matsmi[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_matsmi[ll_fila], '00.00')
			END IF 
			
			IF ISNULL(dw_madcoschare.Object.mcor_matsma[ll_fila])THEN
				ls_Registro	+= Fill(' ',5)
			ELSE	
				ls_Registro	+=	String(dw_madcoschare.Object.mcor_matsma[ll_fila], '00.00')
			END IF
						
			li_filadet	=	dw_registro.InsertRow(0)
			
			dw_registro.Object.registro[li_filadet]	=	ls_Registro
			
		NEXT
		
		sle_mensaje.text	= "Archivo " +	"Movimiento generando madurez de cosecha"
	END IF
						
	//COLOR DE CUBRIMIENTO
	li_color	=	dw_colcubcatre.Retrieve(li_planta,li_especie,ll_lote,li_variedad)
	FOR ll_fila = 1 TO li_color	
		ls_Registro =	"7"

		IF ISNULL(dw_colcubcatre.Object.lote_pltcod[ll_fila])THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro	+=	String(dw_colcubcatre.Object.lote_pltcod[ll_fila], '0000')
		END IF
		
		IF ISNULL(dw_colcubcatre.Object.lote_espcod[ll_fila])THEN
			ls_Registro	+= Fill(' ',2)
		ELSE	
			ls_Registro	+=	String(dw_colcubcatre.Object.lote_espcod[ll_fila], '00')
		END IF
			
		IF ISNULL(dw_colcubcatre.Object.lote_codigo[ll_fila])THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro	+=	String(dw_colcubcatre.Object.lote_codigo[ll_fila], '0000')
		END IF 

		IF ISNULL(dw_colcubcatre.Object.cate_codigo[ll_fila]) THEN
			ls_Registro	+= Fill(' ',3)
		ELSE	
			ls_Registro	+=	String(dw_colcubcatre.Object.cate_codigo[ll_fila], '000')
		END IF
		
		IF ISNULL(dw_colcubcatre.Object.ccca_porcen[ll_fila])THEN
			ls_Registro	+= Fill(' ',5)
		ELSE	
			ls_Registro	+=	String(dw_colcubcatre.Object.ccca_porcen[ll_fila], '00.00')
		END IF 
		
		li_filadet	=	dw_registro.InsertRow(0)
		dw_registro.Object.registro[li_filadet]	=	ls_Registro
	NEXT
	sle_mensaje.text	= "Archivo " +	"Movimiento generando color de cubrimiento"

	//DISTRIUCIÓN DE CALIBRES
	li_calibre	=	dw_distcalibre.Retrieve(li_planta,li_especie,ll_lote,li_variedad)
	FOR ll_fila = 1 TO li_calibre			
		ls_Registro =	"8"

		IF ISNULL(dw_distcalibre.Object.lote_pltcod[ll_fila])THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro	+=	String(dw_distcalibre.Object.lote_pltcod[ll_fila], '0000')
		END IF
		
		IF ISNULL(dw_distcalibre.Object.lote_espcod[ll_fila])THEN
			ls_Registro	+= Fill(' ',2)
		ELSE	
			ls_Registro	+=	String(dw_distcalibre.Object.lote_espcod[ll_fila], '00')
		END IF
		
		IF ISNULL(dw_distcalibre.Object.lote_codigo[ll_fila])THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro	+=	String(dw_distcalibre.Object.lote_codigo[ll_fila], '0000')
		END IF 			
		
		IF ISNULL(dw_distcalibre.Object.disca_grupca[ll_fila])THEN
			ls_Registro	+= Fill(' ',3)
		ELSE	
			ls_Registro	+=	dw_distcalibre.Object.disca_grupca[ll_fila] + &
								Fill(' ',3-Len(String(dw_distcalibre.Object.disca_grupca[ll_fila])))
		END IF 			
			
		IF ISNULL(dw_distcalibre.Object.disca_porren[ll_fila])THEN
			ls_Registro	+= Fill(' ',5)
		ELSE	
			ls_Registro	+=	String(dw_distcalibre.Object.disca_porren[ll_fila], '00.00')
		END IF 			
			 
		li_filadet	=	dw_registro.InsertRow(0)
		dw_registro.Object.registro[li_filadet]	=	ls_Registro
	NEXT
	
	sle_mensaje.text	= "Archivo " +	"Movimiento Generando Distribución de Calibres"

	//DAÑOS Y DEFECTOS
	li_dadefe	=	dw_dyfre.Retrieve(li_planta,li_especie,ll_lote,li_variedad)
	FOR ll_fila = 1 TO li_dadefe	
		ls_Registro =	"9"

		IF ISNULL(dw_dyfre.Object.lote_pltcod[ll_fila])THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro	+=	String(dw_dyfre.Object.lote_pltcod[ll_fila], '0000')
		END IF
		
		IF ISNULL(dw_dyfre.Object.lote_espcod[ll_fila])THEN
			ls_Registro	+= Fill(' ',2)
		ELSE	
			ls_Registro	+=	String(dw_dyfre.Object.lote_espcod[ll_fila], '00')
		END IF

		IF ISNULL(dw_dyfre.Object.lote_codigo[ll_fila])THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro	+=	String(dw_dyfre.Object.lote_codigo[ll_fila], '0000')
		END IF

		IF ISNULL(dw_dyfre.Object.dade_tipodd[ll_fila])THEN
			ls_Registro	+= Fill(' ',1)
		ELSE	
			ls_Registro	+=	String(dw_dyfre.Object.dade_tipodd[ll_fila], '0')
		END IF
		
		IF ISNULL(dw_dyfre.Object.dade_codigo[ll_fila])THEN
			ls_Registro	+= Fill(' ',2)
		ELSE	
			ls_Registro	+=	String(dw_dyfre.Object.dade_codigo[ll_fila], '00')
		END IF

		IF ISNULL(dw_dyfre.Object.dade_podade[ll_fila])THEN
			ls_Registro	+= Fill(' ',5)
		ELSE	
			ls_Registro	+=	String(dw_dyfre.Object.dade_podade[ll_fila], '00,00')
		END IF 
		
		IF ISNULL(dw_dyfre.Object.dade_poddpr[ll_fila])THEN
			ls_Registro	+= Fill(' ',5)
		ELSE	
			ls_Registro	+=	String(dw_dyfre.Object.dade_poddpr[ll_fila], '00,00')
		END IF
		
		li_filadet	=	dw_registro.InsertRow(0)
		dw_registro.Object.registro[li_filadet]	=	ls_Registro
	NEXT
	
	sle_mensaje.text	= "Archivo " +	"Movimiento generando Daños y Defectos"	

	//COLOR DE FONDO
	dw_colorfondorec.GetChild("cofo_codigo", idwc_colorfondo)
	idwc_colorfondo.SetTransObject(SQLCA)
	idwc_colorfondo.Retrieve(li_especie,li_variedad)
	
	dw_colorfondorec.InsertRow(0)
	li_colorfon	=	dw_colorfondorec.Retrieve(li_planta,li_especie,ll_lote,li_variedad)
	
	FOR ll_fila = 1 TO li_colorfon	
		ls_Registro =	"A"

		IF ISNULL(dw_colorfondorec.Object.lote_pltcod[ll_fila])THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro	+=	String(dw_colorfondorec.Object.lote_pltcod[ll_fila], '0000')
		END IF
		
		IF ISNULL(dw_colorfondorec.Object.lote_espcod[ll_fila])THEN
			ls_Registro	+= Fill(' ',2)
		ELSE	
			ls_Registro	+=	String(dw_colorfondorec.Object.lote_espcod[ll_fila], '00')
		END IF
		
		IF ISNULL(dw_colorfondorec.Object.lote_codigo[ll_fila])THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro	+=	String(dw_colorfondorec.Object.lote_codigo[ll_fila], '0000')
		END IF 		

		IF ISNULL(dw_colorfondorec.Object.vari_codigo[ll_fila])THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro	+=	String(dw_colorfondorec.Object.vari_codigo[ll_fila], '0000')
		END IF 		
			
		IF ISNULL(dw_colorfondorec.Object.cofo_codigo[ll_fila])THEN
			ls_Registro	+= Fill(' ',2)
		ELSE	
			ls_Registro	+=	String(dw_colorfondorec.Object.cofo_codigo[ll_fila], '00')
		END IF 		

		IF ISNULL(dw_colorfondorec.Object.cfre_porcol[ll_fila])THEN
			ls_Registro	+= Fill(' ',5)
		ELSE	
			ls_Registro	+=	String(dw_colorfondorec.Object.cfre_porcol[ll_fila], '00,00')
		END IF

		li_filadet	=	dw_registro.InsertRow(0)
		dw_registro.Object.registro[li_filadet]	=	ls_Registro
		sle_mensaje.text	= "Archivo " +	"Movimiento Generando Color de Fondo"
	NEXT
NEXT



end subroutine

public subroutine genera_traspaso ();////
end subroutine

public subroutine existemovimiento (integer ai_planta, integer ai_tipomov, long al_numero);Date ldt_fecha

SELECT mfge_fecmov  INTO :ldt_fecha
  FROM dba.spro_movtofrutagranenca
 WHERE plde_codigo = :ai_planta
   AND tpmv_codigo = :ai_tipomov
	AND mfge_numero = :al_numero;
	
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Movimiento de Fruta Granel")
   em_nro_despacho.text=""
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Número de Movimiento no Ha Sido Ingresado",Exclamation!)
	em_nro_despacho.text=""
ELSE
   em_fecha.text 			=	String(ldt_fecha,'dd/mm/yyyy')	
END IF



end subroutine

on w_genera_transmision.create
this.rb_despa=create rb_despa
this.rb_prefac=create rb_prefac
this.rb_existencia=create rb_existencia
this.rb_traspaso=create rb_traspaso
this.dw_lotesfgdeta=create dw_lotesfgdeta
this.dw_movtoenvaenca=create dw_movtoenvaenca
this.cb_3=create cb_3
this.st_5=create st_5
this.em_fecha=create em_fecha
this.dw_dyfre=create dw_dyfre
this.dw_colorfondorec=create dw_colorfondorec
this.dw_distcalibre=create dw_distcalibre
this.dw_colcubcatre=create dw_colcubcatre
this.dw_madcoschare=create dw_madcoschare
this.dw_movtoenvadeta=create dw_movtoenvadeta
this.dw_lotesfgenca=create dw_lotesfgenca
this.dw_movtofgenca=create dw_movtofgenca
this.dw_movtofgdeta=create dw_movtofgdeta
this.dw_planta=create dw_planta
this.pb_grabar=create pb_grabar
this.sle_mensaje=create sle_mensaje
this.st_6=create st_6
this.cb_1=create cb_1
this.em_tipomovto=create em_tipomovto
this.em_nro_despacho=create em_nro_despacho
this.st_4=create st_4
this.st_3=create st_3
this.st_2=create st_2
this.st_1=create st_1
this.dw_registro=create dw_registro
this.gb_1=create gb_1
this.pb_salir=create pb_salir
this.gb_2=create gb_2
this.gb_3=create gb_3
this.st_7=create st_7
this.Control[]={this.rb_despa,&
this.rb_prefac,&
this.rb_existencia,&
this.rb_traspaso,&
this.dw_lotesfgdeta,&
this.dw_movtoenvaenca,&
this.cb_3,&
this.st_5,&
this.em_fecha,&
this.dw_dyfre,&
this.dw_colorfondorec,&
this.dw_distcalibre,&
this.dw_colcubcatre,&
this.dw_madcoschare,&
this.dw_movtoenvadeta,&
this.dw_lotesfgenca,&
this.dw_movtofgenca,&
this.dw_movtofgdeta,&
this.dw_planta,&
this.pb_grabar,&
this.sle_mensaje,&
this.st_6,&
this.cb_1,&
this.em_tipomovto,&
this.em_nro_despacho,&
this.st_4,&
this.st_3,&
this.st_2,&
this.st_1,&
this.dw_registro,&
this.gb_1,&
this.pb_salir,&
this.gb_2,&
this.gb_3,&
this.st_7}
end on

on w_genera_transmision.destroy
destroy(this.rb_despa)
destroy(this.rb_prefac)
destroy(this.rb_existencia)
destroy(this.rb_traspaso)
destroy(this.dw_lotesfgdeta)
destroy(this.dw_movtoenvaenca)
destroy(this.cb_3)
destroy(this.st_5)
destroy(this.em_fecha)
destroy(this.dw_dyfre)
destroy(this.dw_colorfondorec)
destroy(this.dw_distcalibre)
destroy(this.dw_colcubcatre)
destroy(this.dw_madcoschare)
destroy(this.dw_movtoenvadeta)
destroy(this.dw_lotesfgenca)
destroy(this.dw_movtofgenca)
destroy(this.dw_movtofgdeta)
destroy(this.dw_planta)
destroy(this.pb_grabar)
destroy(this.sle_mensaje)
destroy(this.st_6)
destroy(this.cb_1)
destroy(this.em_tipomovto)
destroy(this.em_nro_despacho)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.dw_registro)
destroy(this.gb_1)
destroy(this.pb_salir)
destroy(this.gb_2)
destroy(this.gb_3)
destroy(this.st_7)
end on

event open;x	=	0
y	=	0

This.Icon	=	Gstr_apl.Icono

em_nro_despacho.setfocus()

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.Retrieve(gi_codexport)
dw_planta.InsertRow(0)
dw_planta.SetItem(1,"plde_codigo",gstr_paramplanta.CodigoPlanta)
//
istr_mant.argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
//

dw_movtofgenca.SetTransObject(Sqlca)
dw_movtofgdeta.SetTransObject(Sqlca)
dw_lotesfgenca.SetTransObject(sqlca)
dw_lotesfgdeta.SetTransObject(sqlca)
dw_movtoenvaenca.SetTransObject(Sqlca)
dw_movtoenvadeta.SetTransObject(Sqlca)
dw_colorfondorec.SetTransObject(Sqlca)
dw_madcoschare.SetTransObject(Sqlca)
dw_colcubcatre.SetTransObject(Sqlca)
dw_distcalibre.SetTransObject(Sqlca)
dw_dyfre.SetTransObject(Sqlca)
dw_movtoenvadeta.SetTransObject(Sqlca)
dw_registro.SetTransObject(Sqlca)
end event

event mousemove;IF(IsValid(w_main))Then
	w_main.SetMicroHelp("Ventana : " + ClassName())
End if

end event

type rb_despa from radiobutton within w_genera_transmision
integer x = 2135
integer y = 324
integer width = 402
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Despachos"
end type

type rb_prefac from radiobutton within w_genera_transmision
integer x = 1417
integer y = 324
integer width = 544
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Pre-Facturación"
end type

type rb_existencia from radiobutton within w_genera_transmision
integer x = 914
integer y = 324
integer width = 402
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Existencia"
end type

type rb_traspaso from radiobutton within w_genera_transmision
integer x = 283
integer y = 324
integer width = 549
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Traspaso Planta"
boolean checked = true
end type

type dw_lotesfgdeta from datawindow within w_genera_transmision
boolean visible = false
integer x = 1047
integer y = 1508
integer width = 901
integer height = 88
integer taborder = 70
string dataobject = "dw_lotesfrutagrandeta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_movtoenvaenca from datawindow within w_genera_transmision
boolean visible = false
integer x = 133
integer y = 1412
integer width = 901
integer height = 88
integer taborder = 50
string dataobject = "dw_mant_movtoenvaenca"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type cb_3 from commandbutton within w_genera_transmision
integer x = 1134
integer y = 632
integer width = 87
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;//Str_Busqueda	lstr_busq
//
//lstr_Busq.Argum[1]	=	string(dw_planta.Object.plde_codigo[1])
//lstr_Busq.Argum[2]	=	"22"
//lstr_Busq.Argum[3]	=	"3"
//lstr_Busq.Argum[4]	=	""
//
//OpenWithParm(w_busc_spro_movtofrutagranenca, lstr_busq)
//
//lstr_busq	=	Message.PowerObjectParm
//
//IF lstr_Busq.Argum[1] <> "" THEN
//	em_nro_despacho.text	=	lstr_Busq.Argum[3]
//	em_fecha.text 			=	mid(lstr_Busq.Argum[4],1,10)
//END IF
end event

type st_5 from statictext within w_genera_transmision
integer x = 1733
integer y = 636
integer width = 197
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Fecha"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_genera_transmision
integer x = 1957
integer y = 624
integer width = 402
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
alignment alignment = center!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type dw_dyfre from datawindow within w_genera_transmision
boolean visible = false
integer x = 133
integer y = 1604
integer width = 901
integer height = 88
string title = "none"
string dataobject = "dw_mues_spro_danoydefectosre"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_colorfondorec from datawindow within w_genera_transmision
boolean visible = false
integer x = 1047
integer y = 1700
integer width = 901
integer height = 88
string dataobject = "dw_mues_spro_colordefondorec_genera"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_distcalibre from datawindow within w_genera_transmision
boolean visible = false
integer x = 133
integer y = 1700
integer width = 901
integer height = 88
string dataobject = "dw_mues_spro_distribcalibresre"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_colcubcatre from datawindow within w_genera_transmision
boolean visible = false
integer x = 133
integer y = 1796
integer width = 901
integer height = 88
string title = "none"
string dataobject = "dw_mues_spro_colcubcategoriaare"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_madcoschare from datawindow within w_genera_transmision
boolean visible = false
integer x = 1047
integer y = 1604
integer width = 901
integer height = 88
string dataobject = "dw_mues_spro_madurezcosechare_genera"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_movtoenvadeta from datawindow within w_genera_transmision
boolean visible = false
integer x = 1047
integer y = 1412
integer width = 901
integer height = 88
string title = "none"
string dataobject = "dw_mues_movtoenvadeta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_lotesfgenca from datawindow within w_genera_transmision
boolean visible = false
integer x = 133
integer y = 1508
integer width = 901
integer height = 88
string title = "none"
string dataobject = "dw_lote_detalle"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_movtofgenca from datawindow within w_genera_transmision
boolean visible = false
integer x = 133
integer y = 1316
integer width = 901
integer height = 88
integer taborder = 80
string title = "none"
string dataobject = "dw_registro_genera"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_movtofgdeta from datawindow within w_genera_transmision
boolean visible = false
integer x = 1047
integer y = 1316
integer width = 901
integer height = 88
integer taborder = 60
string title = "none"
string dataobject = "dw_mues_movtofrutagraneldeta_despacho"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_planta from datawindow within w_genera_transmision
string tag = "Planta"
integer x = 768
integer y = 512
integer width = 869
integer height = 92
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_null

IF Not iuo_plantadesp.existe(Integer(data),True,SQLCa) THEN
	dw_planta.SetItem(1,"plde_codigo",SetNull(li_null))
	Return 1
END IF	
end event

event itemerror;Return 1
end event

type pb_grabar from picturebutton within w_genera_transmision
integer x = 3095
integer y = 524
integer width = 155
integer height = 140
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "F:\Desarrollo\Bmp\Disksave.bmp"
alignment htextalign = left!
end type

event clicked;Parent.triggerevent("ue_guardar")
pb_salir.setfocus()
end event

type sle_mensaje from singlelineedit within w_genera_transmision
integer x = 224
integer y = 1164
integer width = 2569
integer height = 84
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
end type

type st_6 from statictext within w_genera_transmision
integer x = 210
integer y = 1040
integer width = 261
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Mensaje"
alignment alignment = center!
boolean focusrectangle = false
end type

type cb_1 from commandbutton within w_genera_transmision
integer x = 114
integer y = 1004
integer width = 2816
integer height = 332
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
end type

type em_tipomovto from editmask within w_genera_transmision
boolean visible = false
integer x = 1614
integer y = 1804
integer width = 338
integer height = 84
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string text = "22"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "##"
end type

type em_nro_despacho from editmask within w_genera_transmision
integer x = 768
integer y = 624
integer width = 343
integer height = 92
integer taborder = 10
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

event modified;existemovimiento(dw_planta.Object.plde_codigo[1],integer(em_tipomovto.text), &
                 long(this.text))
end event

type st_4 from statictext within w_genera_transmision
integer x = 210
integer y = 636
integer width = 544
integer height = 68
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Número Despacho"
boolean focusrectangle = false
end type

type st_3 from statictext within w_genera_transmision
integer x = 210
integer y = 524
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_genera_transmision
integer x = 119
integer y = 60
integer width = 2811
integer height = 100
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Transmisión de Datos "
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_1 from statictext within w_genera_transmision
integer x = 114
integer y = 476
integer width = 2816
integer height = 528
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_registro from datawindow within w_genera_transmision
boolean visible = false
integer x = 1065
integer y = 1800
integer width = 526
integer height = 84
string title = "none"
string dataobject = "dw_registro_traspaso"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type gb_1 from groupbox within w_genera_transmision
integer x = 3026
integer y = 712
integer width = 293
integer height = 284
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type pb_salir from picturebutton within w_genera_transmision
integer x = 3095
integer y = 800
integer width = 155
integer height = 140
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "F:\Desarrollo\Bmp\Exit.bmp"
alignment htextalign = left!
end type

event clicked;close(w_genera_transmision)
end event

type gb_2 from groupbox within w_genera_transmision
integer x = 3031
integer y = 452
integer width = 288
integer height = 256
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_3 from groupbox within w_genera_transmision
integer x = 155
integer y = 208
integer width = 2693
integer height = 232
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Selección  a Transmitir"
end type

type st_7 from statictext within w_genera_transmision
integer x = 119
integer y = 188
integer width = 2811
integer height = 288
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

