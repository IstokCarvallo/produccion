$PBExportHeader$w_mant_mues_spro_cajasprod.srw
$PBExportComments$Ingreso de Documentos Internos Packing
forward
global type w_mant_mues_spro_cajasprod from window
end type
type sle_lectura from singlelineedit within w_mant_mues_spro_cajasprod
end type
type pb_grafico from picturebutton within w_mant_mues_spro_cajasprod
end type
type st_advertencia from statictext within w_mant_mues_spro_cajasprod
end type
type dw_5 from datawindow within w_mant_mues_spro_cajasprod
end type
type dw_1 from datawindow within w_mant_mues_spro_cajasprod
end type
type pb_ok from picturebutton within w_mant_mues_spro_cajasprod
end type
type pb_salir from picturebutton within w_mant_mues_spro_cajasprod
end type
type gb_1 from groupbox within w_mant_mues_spro_cajasprod
end type
type gb_2 from groupbox within w_mant_mues_spro_cajasprod
end type
type gb_4 from groupbox within w_mant_mues_spro_cajasprod
end type
type gb_5 from groupbox within w_mant_mues_spro_cajasprod
end type
type st_1 from statictext within w_mant_mues_spro_cajasprod
end type
type dw_embala from datawindow within w_mant_mues_spro_cajasprod
end type
type dw_calibr from datawindow within w_mant_mues_spro_cajasprod
end type
type rb_l1 from radiobutton within w_mant_mues_spro_cajasprod
end type
type rb_l2 from radiobutton within w_mant_mues_spro_cajasprod
end type
type gb_3 from groupbox within w_mant_mues_spro_cajasprod
end type
type dw_4 from datawindow within w_mant_mues_spro_cajasprod
end type
type dw_7 from datawindow within w_mant_mues_spro_cajasprod
end type
type dw_2 from datawindow within w_mant_mues_spro_cajasprod
end type
end forward

global type w_mant_mues_spro_cajasprod from window
string tag = "w_mant_mues_spro_cajasprod"
integer width = 3141
integer height = 1544
boolean titlebar = true
string title = "Cajas en Producción"
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 12632256
boolean center = true
event ue_guardar pbm_custom11
event ue_imprimir ( )
event ue_antesguardar ( )
event ue_validaguardar ( )
event ue_modifica ( )
event ue_imprimirerror ( )
sle_lectura sle_lectura
pb_grafico pb_grafico
st_advertencia st_advertencia
dw_5 dw_5
dw_1 dw_1
pb_ok pb_ok
pb_salir pb_salir
gb_1 gb_1
gb_2 gb_2
gb_4 gb_4
gb_5 gb_5
st_1 st_1
dw_embala dw_embala
dw_calibr dw_calibr
rb_l1 rb_l1
rb_l2 rb_l2
gb_3 gb_3
dw_4 dw_4
dw_7 dw_7
dw_2 dw_2
end type
global w_mant_mues_spro_cajasprod w_mant_mues_spro_cajasprod

type variables
str_mant					istr_mant

uo_predios        	iuo_predio
uo_embalajesprod 		iuo_embalajesprod
uo_buscadatosproceso	iuo_buscadatosproceso
uo_lotescorrelequipo	iuo_correl
uo_especie				iuo_especie

Long						ii_proceso, ii_lote, il_NroCaja, il_maximo
String					is_embalaje, is_calibre, is_Computador, is_equipotrans
Integer					ii_zona, ii_Cliente, ii_Planta, ii_linea, ii_lineapack, ii_cliente2
Date						id_Fecha

Boolean					ib_MultiEmbalaje, ib_MultiCalibre, 	ib_respuesta, 	ib_Bloqueo
String					is_proceso, 		is_lote, 			is_embala, 		is_calib
Integer					ii_cajaspallet, 	ii_totalpallet, 	ii_categoria, 	ii_salida, 	ii_especie
Long						il_loco_nropal, 	il_lotemenor, 		il_salidatrans
Integer					ii_lineatrans
end variables

forward prototypes
protected function boolean wf_actualiza_db ()
public function boolean despieceregistro (string as_registro)
public function boolean buscadatosproceso (long al_proceso)
public function boolean buscanuevocorrelativo (integer ai_bodega, integer ai_cliente)
public subroutine buscanuevocorrelativoerrores (integer ai_bodega, integer ai_cliente)
public function long loteproceso (integer ai_tipo, long al_numero, integer ai_cliente, integer ai_planta)
public function boolean cargaprograma ()
public subroutine palletactual ()
public function boolean validaprocedimiento ()
public function boolean validamultiprograma ()
public function boolean cargaparametros ()
public subroutine existeprogramacion ()
end prototypes

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN -1

SetPointer(HourGlass!)

Message.DoubleParm = 0

IF Message.DoubleParm = -1 THEN RETURN -1

IF wf_actualiza_db() THEN
		
ELSE		
	Message.DoubleParm = -1
	RETURN -1
END IF

RETURN 0
end event

event ue_imprimir();SetPointer(HourGlass!)

String	ls_Especie, ls_Variedad, ls_Categoria, ls_Codigo1, ls_Codigo2, &
			ls_Codigo3, ls_FDAProd, ls_Packing, ls_ComunaPack, ls_ProvinPack, &
			ls_Calibre, ls_Predio, ls_ProdComuna, ls_ProdProvincia, ls_Embalaje, &
			ls_ComuProvPack, ls_plde_razsoc, ls_envo
Integer	li_Productor, li_Predio, li_Cuartel, li_Cuarte1, li_Huerto, &
         li_Etiqueta, li_nrlote, li_Cliente, li_Planta, li_Especie, &
			li_Region, li_Provin, li_Comuna
Long		ll_Fila, ll_Trabajo, ll_numero, ll_productor
Decimal	ld_KgsNeto, ld_LbsNeto, li_envo
Date		ld_FechaEmbalaje

ll_Fila = dw_2.Retrieve(dw_1.Object.clie_codigo[1], &
								dw_1.Object.plde_codigo[1], &
								dw_1.Object.capr_numero[1], &
								dw_1.Object.capr_numero[1], 1)
					
CHOOSE CASE gs_Impresora
	CASE "Zebra","AccuMax"
		
		IF IsNull(dw_2.Object.prod_huerto[1]) THEN li_Huerto = 1
		IF IsNull(dw_2.Object.prod_cuarte[1]) THEN li_Cuarte1 = 1
		IF IsNull(dw_2.Object.etiq_codigo[1]) OR &
			dw_2.Object.etiq_codigo[1] = 0 THEN li_Etiqueta = 1		
		
		ls_Codigo1	 		=	'01278' + String(dw_2.Object.clie_codigo[1], '000') + &
									String(dw_2.Object.espe_codigo[1], '00000') + &
									String(dw_2.Object.vari_codigo[1], '00') + '010' + &
									String(dw_2.Object.plde_codigo[1], '0000') + &
									String(dw_2.Object.capr_fecemb[1], 'ddmmyy') + &
									dw_2.Object.emba_codigo[1]
									
		ls_Codigo2	 		=	'93'+ String(dw_2.Object.prod_codigo[1] ,'0000')+ & 
									String(li_Huerto ,'000')+ &
									String(li_Cuarte1 ,'000')+ &
									dw_2.Object.capr_calibr[1] + &
									String(li_Etiqueta ,'00') 
									
		ls_Codigo3	 		=	String(dw_2.Object.zona_codigo[1], '00') + &
									String(dw_2.Object.plde_codigo[1], '0000') + &
									String(dw_1.Object.capr_numero[1], '0000000000')
									
		ls_Variedad  		=  dw_2.Object.vari_nombre[1]
		li_Especie			=	dw_2.Object.espe_codigo[1]
		ls_Especie   		=  dw_2.Object.espe_noming[1]
		ld_KgsNeto  		=  dw_2.Object.enva_pesone[1]
		ld_LbsNeto  		=	Round(dw_2.Object.enva_pesone[1] * 2.2046,0)
		ls_FDAProd   		=  String(dw_2.Object.plde_insfda[1])
		ls_Packing   		=  dw_2.Object.plde_nombre[1]
		
		ls_ComunaPack  	=  dw_2.Object.comu_nombre[1]
		ls_ProvinPack		=	dw_2.Object.prov_nombre[1]
		ls_ComuProvPack	=	ls_ComunaPack + "-" + ls_ProvinPAck
		
		ls_Calibre     	=  dw_2.Object.capr_calibr[1]
		li_Productor   	=  dw_2.Object.prod_codigo[1]
		li_Cuartel     	=  dw_2.Object.prod_cuarte[1]
		ls_ProdComuna  	=  dw_2.Object.prod_comuna[1]
		ls_ProdProvincia  =  dw_2.Object.prod_provin[1]
		ls_Embalaje    	=  dw_2.Object.emba_codigo[1]
		ld_FechaEmbalaje	=  dw_2.Object.capr_fecemb[1]
		//ls_Predio			=	dw_2.Object.xxxxx[1] + " / C" + String(li_Predio)
		ls_plde_razsoc		=  dw_2.Object.plde_razsoc[1]
		li_nrlote			=	dw_2.Object.capr_nrlote[1]
		ll_productor		=	dw_2.Object.prod_codigo[1]
		ls_envo				=	dw_2.Object.envo_descrip[1]
		ls_Predio			=	dw_2.Object.prpr_nombre[1]
		li_Predio			=	dw_2.Object.prbr_codpre[1]
		li_cuartel			=	dw_2.Object.prcc_codigo[1]
		
		IF ls_Categoria = "" OR IsNull(ls_Categoria) THEN
			ls_Categoria = 'CAT 1'
		END IF
		
		IF IsNull(li_Predio) THEN li_Predio	=  dw_2.Object.prod_predio[1]
		
		li_Cliente 	=	dw_1.Object.clie_codigo[1]
		li_Planta	=	dw_1.Object.plde_codigo[1]
		ll_Numero	=	dw_1.Object.capr_numero[1]
		
		IF Trim(ls_Predio) = "" THEN 
			ls_Predio	=  '0001 / C001'
			li_Predio   =  1
		ELSE
			ls_Predio	=  ls_Predio+" / C"+String(li_cuartel,'000')
		END IF
		
	CASE "Zebra2600-300"
		ll_Trabajo	=	PrintOpen()
		
		Print(ll_Trabajo, "^XA")
		Print(ll_Trabajo, "^FO90,400^A0B,55,46^CI13^FR^FD" + ls_Variedad + "^FS")
		Print(ll_Trabajo, "^FO94,45^A0B,40,37^CI13^FR^FD"+ ls_Categoria + "^FS")
		Print(ll_Trabajo, "^FO150,413^A0B,39,37^CI13^FR^FD" + ls_Especie + "^FS")
		Print(ll_Trabajo, "^FO195,350^A0B,39,32^CI13^FR^FDNet Weight: " + &
								Trim(String(ld_KgsNeto, "0.0")) + " Kg - " + &
								Trim(String(ld_LbsNeto, "##0.0")) + " Lb^FS")
		Print(ll_Trabajo, "^FO271,727^A0B,23,19^CI13^FR^FDFDA CODE:^FS")
		Print(ll_Trabajo, "^FO234,390^A0B,30,25^CI13^FR^FDPACKING IDENTIFICATION^FS")
		Print(ll_Trabajo, "^FO307,740^A0B,23,19^CI13^FR^FDNOMBRE:^FS")
		Print(ll_Trabajo, "^FO367,698^A0B,23,19^CI13^FR^FDComuna/Prov:  ^FS")
		Print(ll_Trabajo, "^FO273,546^A0B,32,26^CI13^FR^FD" + ls_FDAProd + "^FS")
		Print(ll_Trabajo, "^FO308,350^A0B,28,23^CI13^FR^FD"+ls_plde_razsoc+"^FS")
		Print(ll_Trabajo, "^FO336,448^A0B,28,23^CI13^FR^FD" + ls_Packing + "^FS")
		Print(ll_Trabajo, "^FO368,482^A0B,28,23^CI13^FR^FD" + ls_ComuProvPack + "^FS")
		Print(ll_Trabajo, "^FO414,390^A0B,30,25^CI13^FR^FDGROWER IDENTIFICATION^FS")
		Print(ll_Trabajo, "^FO447,753^A0B,23,19^CI13^FR^FDCODE:   ^FS")
		Print(ll_Trabajo, "^FO440,237^A0B,47,39^CI13^FR^FD" + &
								String(dw_2.Object.vari_codigo[1], '00') + "^FS")
		Print(ll_Trabajo, "^FO507,748^A0B,23,19^CI13^FR^FDPREDIO: ^FS")
		Print(ll_Trabajo, "^FO543,698^A0B,23,19^CI13^FR^FDComuna/Prov:  ^FS")
		Print(ll_Trabajo, "^FO544,472^A0B,28,23^CI13^FR^FD" + ls_ProdComuna + " / " + &
								ls_ProdProvincia + "^FS")
		Print(ll_Trabajo, "^FO176,64^A0B,39,32^CI13^FR^FD"+ls_envo+"^FS")
		Print(ll_Trabajo, "^FO296,50^A0B,55,46^CI13^FR^FD" + ls_Calibre + "^FS")
		Print(ll_Trabajo, "^FO430,41^A0B,55,46^CI13^FR^FD" + ls_Embalaje + "^FS")
		Print(ll_Trabajo, "^FO522,37^A0B,39,32^CI13^FR^FD" + &
								String(ld_FechaEmbalaje, 'ddmmyy') + "^FS")
		Print(ll_Trabajo, "^BY3,3.0^FO590,80^BCB,88,Y,N,N^FR^FD>:" + ls_Codigo3 + "^FS")
		Print(ll_Trabajo, "^FO444,525^A0B,48,39^CI13^FR^FD" + String(li_Productor, '0000') + &
								" / " + String(li_Predio, '000') + "^FS")
		Print(ll_Trabajo, "^FO508,472^A0B,28,23^CI13^FR^FD" + ls_Predio + "^FS")
		Print(ll_Trabajo, "^FO138,2^GB0,847,2^FS")
		Print(ll_Trabajo, "^FO226,2^GB0,841,2^FS")
		Print(ll_Trabajo, "^FO406,2^GB0,841,2^FS")
		Print(ll_Trabajo, "^FO578,2^GB0,841,2^FS")
		Print(ll_Trabajo, "^FO74,182^GB504,0,2^FS")
		Print(ll_Trabajo, "^FO498,2^GB0,180,2^FS")
		Print(ll_Trabajo, "^XZ")
		
		PrintClose(ll_Trabajo)
		
	CASE "AccuMax"
		ll_Trabajo	=	PrintOpen()
		
      Print(ll_Trabajo, "^@60,3")
		Print(ll_Trabajo, "^W70")
		Print(ll_Trabajo, "^H10")
		Print(ll_Trabajo, "^P1")
		Print(ll_Trabajo, "^S4")
		Print(ll_Trabajo, "^AT")
		Print(ll_Trabajo, "^C1")
		Print(ll_Trabajo, "^R0")
		Print(ll_Trabajo, "~Q+0")
		Print(ll_Trabajo, "^O0")
		Print(ll_Trabajo, "^D0")
		Print(ll_Trabajo, "^E14")
		Print(ll_Trabajo, "~R200")
		Print(ll_Trabajo, "^%")
		Print(ll_Trabajo, "")
		Print(ll_Trabajo, "Dy2-me-dd")
		Print(ll_Trabajo, "Th:m:s")
		Print(ll_Trabajo, "BQ,379,400,2,6,60,3,1," + ls_Codigo2)
		Print(ll_Trabajo, "BQ,473,360,2,5,40,3,1," + ls_Codigo3)
		Print(ll_Trabajo, "AE,4,355,1,1,0,3," + ls_Variedad)
		Print(ll_Trabajo, "AC,48,358,1,1,0,3,"+ ls_Especie)
		Print(ll_Trabajo, "AA,76,426,1,1,0,3,Net Weight: " + &
								Trim(String(ld_KgsNeto, "0.0")) + " KG - " + &
								Trim(String(ld_LbsNeto, "##0")) + " LBS. NET")
		Print(ll_Trabajo, "AB,10,75,1,1,0,3," + ls_Categoria)
		Print(ll_Trabajo, "AD,138,96,1,1,0,0,")
		Print(ll_Trabajo, "AE,122,14,1,1,0,0,")
		Print(ll_Trabajo, "BQ,258,470,2,6,59,3,1," + ls_Codigo1)
		Print(ll_Trabajo, "Lo,48,0,48,478")
		Print(ll_Trabajo, "Lo,0,79,249,80")
		Print(ll_Trabajo, "Lo,96,0,96,478")
		Print(ll_Trabajo, "AB,59,66,1,1,0,3," + ls_envo)
		Print(ll_Trabajo, "AA,98,396,1,1,0,3,PACKING IDENTIFICATION")
		Print(ll_Trabajo, "AA,116,478,1,1,0,3,FDA Code")
		Print(ll_Trabajo, "AA,134,478,1,1,0,3,Nombre")
		Print(ll_Trabajo, "AA,152,478,1,1,0,3,Comuna / Provincia")
		Print(ll_Trabajo, "AA,118,311,1,1,0,3," + ls_FDAProd)
		Print(ll_Trabajo, "AA,136,311,1,1,0,3," + ls_Packing)
		Print(ll_Trabajo, "AA,153,311,1,1,0,3," + ls_ComunaPack)
		Print(ll_Trabajo, "Lo,176,0,176,478")
		Print(ll_Trabajo, "AD,120,70,1,1,0,3," + ls_Calibre)
		Print(ll_Trabajo, "AA,180,398,1,1,0,3,GROWER IDENTIFICATION")
		Print(ll_Trabajo, "AA,196,478,1,1,0,3,CODE")
		Print(ll_Trabajo, "AB,194,312,1,1,0,3," + String(li_Productor, '0000') + &
								" / " + String(li_Predio, '000'))
		Print(ll_Trabajo, "AA,212,478,1,1,0,3,Predio")
		Print(ll_Trabajo, "AA,213,311,1,1,0,3," + ls_Predio + " / C" + &
								String(li_Cuartel, '001'))
		Print(ll_Trabajo, "AA,226,478,1,1,0,3,Comuna / Provincia")
		Print(ll_Trabajo, "AA,227,311,1,1,0,3," + ls_ProdComuna + " / " + &
								ls_ProdProvincia)
		Print(ll_Trabajo, "Lo,248,0,248,478")
		Print(ll_Trabajo, "Lo,221,47,222,48")
		Print(ll_Trabajo, "Lo,216,0,216,79")
		Print(ll_Trabajo, "AC,181,78,1,1,0,3," + ls_Embalaje)
		Print(ll_Trabajo, "AB,217,79,1,1,0,3," + String(ld_FechaEmbalaje, 'ddmmyy'))
		Print(ll_Trabajo, "$")
		
		PrintClose(ll_Trabajo)
		
   CASE "Zebra_2844"	, "Zebra5200-200", "Zebra2600-200"
		dw_2.Print(False, False)
		
END CHOOSE

SetPointer(Arrow!)
end event

event ue_imprimirerror();SetPointer(HourGlass!)

String	ls_Especie, ls_Variedad, ls_Categoria, ls_Codigo1, ls_Codigo2, &
			ls_Codigo3, ls_FDAProd, ls_Packing, ls_ComunaPack, ls_Calibre, &
			ls_Predio, ls_ProdComuna, ls_ProdProvincia, ls_Embalaje
Integer	li_Productor, li_Predio, li_Cuartel, li_Cuarte1, li_Huerto, li_Etiqueta
Long		ll_Fila, ll_Trabajo, ll_numero
Decimal	ld_KgsNeto, ld_LbsNeto
Date		ld_FechaEmbalaje

//ll_Trabajo	=	PrintOpen()
					
ls_Codigo3	 		=	'090041' + &
                     String(dw_1.Object.capr_numero[1], '0000000000')
							
CHOOSE CASE gs_Impresora
	CASE "Zebra2600-300"
		Print(ll_Trabajo, "^XA")
		Print(ll_Trabajo, "^FO90,248^A0B,55,46^CI13^FR^FDERROR DE LECTURA^FS")
		Print(ll_Trabajo, "^FO94,45^A0B,40,37^CI13^FR^FDERROR^FS")
		Print(ll_Trabajo, "^FO150,413^A0B,39,37^CI13^FR^FDERROR^FS")
		Print(ll_Trabajo, "^FO273,556^A0B,32,26^CI13^FR^FDERROR^FS")
		Print(ll_Trabajo, "^BY3,3.0^FO590,80^BCB,88,Y,N,N^FR^FD>:" + ls_Codigo3 + "^FS")
		Print(ll_Trabajo, "^FO138,2^GB0,847,2^FS")
		Print(ll_Trabajo, "^FO226,2^GB0,841,2^FS")
		Print(ll_Trabajo, "^FO406,2^GB0,841,2^FS")
		Print(ll_Trabajo, "^FO578,2^GB0,841,2^FS")
		Print(ll_Trabajo, "^FO74,182^GB504,0,2^FS")
		Print(ll_Trabajo, "^FO498,2^GB0,180,2^FS")
		Print(ll_Trabajo, "^XZ")
		
	CASE "AccuMax"
      Print(ll_Trabajo, "^@60,3")
		Print(ll_Trabajo, "^W70")
		Print(ll_Trabajo, "^H10")
		Print(ll_Trabajo, "^P1")
		Print(ll_Trabajo, "^S4")
		Print(ll_Trabajo, "^AT")
		Print(ll_Trabajo, "^C1")
		Print(ll_Trabajo, "^R0")
		Print(ll_Trabajo, "~Q+0")
		Print(ll_Trabajo, "^O0")
		Print(ll_Trabajo, "^D0")
		Print(ll_Trabajo, "^E14")
		Print(ll_Trabajo, "~R200")
		Print(ll_Trabajo, "^%")
		Print(ll_Trabajo, "")
		Print(ll_Trabajo, "Dy2-me-dd")
		Print(ll_Trabajo, "Th:m:s")
		Print(ll_Trabajo, "BQ,379,400,2,6,60,3,1," + ls_Codigo2)
		Print(ll_Trabajo, "BQ,473,360,2,5,40,3,1," + ls_Codigo3)
		Print(ll_Trabajo, "AE,4,355,1,1,0,3," + ls_Variedad)
		Print(ll_Trabajo, "AC,48,358,1,1,0,3,"+ ls_Especie)
		Print(ll_Trabajo, "AA,76,426,1,1,0,3,Net Weight: " + &
								Trim(String(ld_KgsNeto, "0.0")) + " KN - " + &
								Trim(String(ld_LbsNeto, "##0")) + " LBS. NET")
		Print(ll_Trabajo, "AB,10,75,1,1,0,3," + ls_Categoria)
		Print(ll_Trabajo, "AD,138,96,1,1,0,0,")
		Print(ll_Trabajo, "AE,122,14,1,1,0,0,")
		Print(ll_Trabajo, "BQ,258,470,2,6,59,3,1," + ls_Codigo1)
		Print(ll_Trabajo, "Lo,48,0,48,478")
		Print(ll_Trabajo, "Lo,0,79,249,80")
		Print(ll_Trabajo, "Lo,96,0,96,478")
		Print(ll_Trabajo, "AB,59,66,1,1,0,3,BAG")
		Print(ll_Trabajo, "AA,98,396,1,1,0,3,PACKING IDENTIFICATION")
		Print(ll_Trabajo, "AA,116,478,1,1,0,3,FDA Code")
		Print(ll_Trabajo, "AA,134,478,1,1,0,3,Nombre")
		Print(ll_Trabajo, "AA,152,478,1,1,0,3,Comuna / Provincia")
		Print(ll_Trabajo, "AA,118,311,1,1,0,3," + ls_FDAProd)
		Print(ll_Trabajo, "AA,136,311,1,1,0,3," + ls_Packing)
		Print(ll_Trabajo, "AA,153,311,1,1,0,3," + ls_ComunaPack)
		Print(ll_Trabajo, "Lo,176,0,176,478")
		Print(ll_Trabajo, "AD,120,70,1,1,0,3," + ls_Calibre)
		Print(ll_Trabajo, "AA,180,398,1,1,0,3,GROWER IDENTIFICATION")
		Print(ll_Trabajo, "AA,196,478,1,1,0,3,CODE")
		Print(ll_Trabajo, "AB,194,312,1,1,0,3," + String(li_Productor, '0000') + &
								" / " + String(li_Predio, '000'))
		Print(ll_Trabajo, "AA,212,478,1,1,0,3,Predio")
		Print(ll_Trabajo, "AA,213,311,1,1,0,3," + ls_Predio + " / C" + &
								String(li_Cuartel, '001'))
		Print(ll_Trabajo, "AA,226,478,1,1,0,3,Comuna / Provincia")
		Print(ll_Trabajo, "AA,227,311,1,1,0,3," + ls_ProdComuna + " / " + &
								ls_ProdProvincia)
		Print(ll_Trabajo, "Lo,248,0,248,478")
		Print(ll_Trabajo, "Lo,221,47,222,48")
		Print(ll_Trabajo, "Lo,216,0,216,79")
		Print(ll_Trabajo, "AC,181,78,1,1,0,3," + ls_Embalaje)
		Print(ll_Trabajo, "AB,217,79,1,1,0,3," + String(ld_FechaEmbalaje, 'ddmmyy'))
		Print(ll_Trabajo, "$")
		
	CASE "Zebra_2844"	, "Zebra5200-200", "Zebra2600-200"
		dw_2.Reset()
		ll_Fila = dw_2.InsertRow(0)
		
		dw_2.Object.espe_noming[ll_Fila] = 'ERROR DE LECTURA'
		dw_2.Print(False, False)
		
END CHOOSE

//PrintClose(ll_Trabajo)

SetPointer(Arrow!)
end event

protected function boolean wf_actualiza_db ();IF dw_1.update() = 1 THEN 
	Commit;
	IF sqlca.sqlcode <> 0 THEN
		F_ErrorBaseDatos(sqlca,This.title)
		Return False
	ELSE
		Return true
	END IF
ELSE
	Rollback;
	IF sqlca.sqlcode <> 0 THEN F_ErrorBaseDatos(sqlca,this.title)
	Return false
END IF

Return True
end function

public function boolean despieceregistro (string as_registro);Integer		li_lectura, li_sigte=1, li_larg, li_lectura2, li_exis1 = 0, li_exis2 = 0, li_exis3 = 0 ,li_exis4 = 0
String		ls_caracter, ls_compone, ls_compone2, ls_mensaje = "", ls_Null

SetNull(ls_Null)

IF gb_programado THEN
	IF NOT CargaPrograma() THEN
		HALT
	END IF
END IF

FOR li_lectura	=	1 TO Len(as_registro)
	ls_caracter =	Mid(as_registro,li_lectura,1)
	ls_compone	=	''
   IF ls_caracter = '&' THEN
			ls_compone = Mid(as_registro,li_lectura + 1,2)
		   li_sigte	=	li_lectura + 3
		   li_larg	=	0
			
		   li_lectura2	=	li_sigte
		   DO WHILE Mid(as_registro,li_sigte,1) <> "&" AND li_sigte <= Len(as_registro)
				li_sigte++
				li_larg++
		   LOOP		 
            ls_compone2 = Mid(as_registro,li_lectura2,li_larg)
		   CHOOSE CASE ls_compone
	
				 CASE "01"
					li_exis1    =  1
					ii_proceso 	= 	Long(ls_compone2)
					dw_1.Object.capr_docrel[1] =  ii_proceso
					
				 CASE "02"
					li_exis2    =  1
					ii_lote    	= 	Long(ls_compone2)	
					dw_1.Object.capr_nrlote[1] =  ii_lote
					
				 CASE "03"
					li_exis3    =  1
					is_embalaje =	ls_compone2					
					dw_1.Object.emba_codigo[1]	=	is_embalaje					
					
				 CASE "04"
					li_exis4    =  1
					is_calibre	=  ls_compone2
					is_calibre  =  is_calibre + '   '
					dw_1.Object.capr_calibr[1]	=	Left(Trim(is_calibre) + '   ',3)
					
				CASE "05"
					IF iuo_especie.quinto = 1 THEN
						ii_categoria =	Integer(ls_compone2)
						dw_1.Object.cate_codigo[1]	=	ii_categoria					
					END IF
			END CHOOSE
			dw_1.Object.capr_estado[1]   = 0			
			li_lectura = li_sigte - 1
	END IF

NEXT
//
//IF li_exis1 = 0 then
//	ls_mensaje = ls_mensaje + "~nProceso "
//END IF
//
//IF li_exis2 = 0 then
//	ls_mensaje = ls_mensaje + "~nLote "
//END IF
//
//IF li_exis3 = 0 then
//	ls_mensaje = ls_mensaje + "~nEmbalaje "
//END IF
//
//IF li_exis4 = 0 then
//	ls_mensaje = ls_mensaje + "~nCalibre "
//END IF
//
//IF ls_mensaje<>"" THEN
//	RETURN FALSE
//ELSE
	RETURN TRUE
//END IF
end function

public function boolean buscadatosproceso (long al_proceso);Integer	li_Planta
String	ls_Null
DateTime	ldt_fecha

SetNull(ls_Null)

IF ii_cliente2 = 0 THEN ii_cliente2 = ii_cliente
IF iuo_BuscaDatosProceso.existe(al_proceso,False,Sqlca, ii_cliente2)	THEN

	iuo_buscadatosproceso.buscapredio(ii_lote,False,Sqlca)
	
	iuo_buscadatosproceso.buscaplanta(iuo_buscadatosproceso.Planta,False,Sqlca)
	
	dw_1.Object.clie_codigo[1]		=		iuo_buscadatosproceso.Cliente
	dw_1.Object.plde_codigo[1]		=		iuo_buscadatosproceso.Planta
	dw_1.Object.capr_fecemb[1] 	=		iuo_buscadatosproceso.Fecha
	dw_1.Object.capr_lineas[1]		=		ii_linea
	dw_1.Object.frio_tipofr[1]		=		iuo_buscadatosproceso.TipoFrio
	
	li_Planta							=		iuo_buscadatosproceso.Planta
	
	IF  iuo_buscadatosproceso.Predio > 0 THEN
		dw_1.Object.prod_predio[1]  	=	iuo_buscadatosproceso.Predio
		dw_1.Object.prod_huerto[1]		=	iuo_buscadatosproceso.Predio
	ELSE
		dw_1.Object.prod_predio[1]   	=    Integer(ls_Null)
		dw_1.Object.prod_huerto[1]   	=    Integer(ls_Null)
	END IF
	 
	IF iuo_buscadatosproceso.Cuartel > 0 THEN
		dw_1.Object.prod_cuarte[1]		=    iuo_buscadatosproceso.Cuartel
	ELSE
		dw_1.Object.prod_cuarte[1]		=    Integer(ls_Null)
	END IF
	
	IF iuo_buscadatosproceso.Packing > 0 THEN
		dw_1.Object.capr_cespak[1]		=    iuo_buscadatosproceso.Packing
	ELSE
		dw_1.Object.capr_cespak[1] 	=    Integer(ls_Null)
	END IF	
	
	IF Not iuo_embalajesprod.Existe(iuo_buscadatosproceso.Cliente,	is_embalaje	,False,Sqlca) THEN
		dw_1.Object.emba_codigo[1]		=	ls_Null
	END IF
	
	IF IsNull(iuo_buscadatosproceso.Productor) THEN
		dw_1.Object.prod_codigo[1]		=	Long(ls_Null)
	ELSE
		dw_1.Object.prod_codigo[1]		=	iuo_buscadatosproceso.Productor
	END IF
	
	IF IsNull(iuo_buscadatosproceso.Variedad) THEN
		dw_1.Object.vari_codigo[1]		=	Integer(ls_Null)
		dw_1.Object.capr_varrot[1]		=	Integer(ls_Null)	
	ELSE
		dw_1.Object.vari_codigo[1]		=	iuo_buscadatosproceso.Variedad
		dw_1.Object.capr_varrot[1]		=	iuo_buscadatosproceso.VarieRotula		
	END IF
	
	ldt_fecha								=	f_fechahora()
	
	dw_1.Object.espe_codigo[1]			=	iuo_buscadatosproceso.Especie
	dw_1.Object.capr_fecdig[1]			=	Date(ldt_fecha)
	dw_1.Object.capr_hordig[1]			=	Time(ldt_fecha)

	IF iuo_especie.quinto <> 1 THEN
		dw_1.Object.cate_codigo[1] 		=	ii_categoria
	END IF
	
	IF IsNull(dw_1.Object.cate_codigo[1]) OR dw_1.Object.cate_codigo[1] = 0 THEN	dw_1.Object.cate_codigo[1] 	=	1
	
	dw_1.Object.etiq_codigo[1] 		=	iuo_buscadatosproceso.Etiqueta
		
END IF

RETURN TRUE
end function

public function boolean buscanuevocorrelativo (integer ai_bodega, integer ai_cliente);IF il_NroCaja < 1 OR IsNull(il_NroCaja) THEN

	IF NOT iuo_correl.Existe(ii_Planta,99, is_Computador, TRUE, sqlca) THEN
		SetNull(il_NroCaja)
		RETURN FALSE
	ELSE
		il_NroCaja	=	iuo_correl.il_correcompa
	END IF

ELSE
	il_NroCaja = il_NroCaja + 1
END IF

dw_1.Object.capr_numero[1]	=	il_NroCaja

RETURN True
end function

public subroutine buscanuevocorrelativoerrores (integer ai_bodega, integer ai_cliente);Long	ll_Numero

SELECT	IsNull(Max(capr_numero), 90000000) + 1
	INTO	:ll_Numero
	FROM	dba.spro_cajasprod
	WHERE	clie_codigo	=	:ai_cliente
	AND	plde_codigo	=	:ai_bodega
	AND	capr_numero >  90000000;

dw_1.Object.capr_numero[1]	=	ll_Numero

RETURN 
end subroutine

public function long loteproceso (integer ai_tipo, long al_numero, integer ai_cliente, integer ai_planta);Long	ll_lote

Select	Max(lote_codigo)
	into:ll_lote
	from dba.spro_ordenprocdeta
	where orpr_tipord		=:ai_tipo
	    and orpr_numero 	=:al_numero
	    and clie_codigo 		=:ai_cliente
	    and plde_codigo	=:ai_planta;

IF sqlca.SQLCode	= -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Programación de Salidas: spro_programasalidas")
	Return -1
ELSEIF sqlca.SQLCode	= 100 THEN
	MessageBox("Error","No existe un Proceso activo en estos momentos.")
	Return -1
ELSE
	Return ll_lote
END IF
end function

public function boolean cargaprograma ();Integer	li_tipo, li_cliente, li_agrupa, li_salidaconso, li_conso, li_lineapack, li_secuen
Long		ll_correl, ll_filas, ll_numero

IF gb_OnFly THEN
	
	SELECT	loco_comlin, loco_nropal
		INTO	:li_lineapack, :il_loco_nropal
		FROM	dba.spro_correlcompequipo
		WHERE	plde_codigo 			= 	:gstr_paramplanta.codigoplanta	
		AND 	line_codigo 			=	:ii_linea
		AND 	Upper(equi_nombre)	= 	Upper(:is_Computador);

		IF isNull(ii_lineapack) OR ii_lineapack = 0 THEN ii_lineapack = li_lineapack

	IF sqlca.SQLCode	= -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de correlativos de compactos: spro_correlcompequipo")
		RETURN FALSE
	ELSEIF sqlca.SQLCode	= 100 THEN
		MessageBox("Error","No existe correlativos de Compactos para el Presente Equipo: " + is_computador)
		RETURN FALSE
	END IF


	SELECT	orpr_tipord, orpr_numero, clie_codigo, prsa_lineaa
		INTO	:li_tipo, :ll_numero, :li_cliente, :li_secuen
		FROM	dba.spro_programasalidas 
		WHERE	prsa_estado	= 	1
		AND	plde_codigo = 	:gstr_paramplanta.codigoplanta
		AND 	prsa_lineaa	=	:ii_lineatrans;

	IF sqlca.SQLCode	= -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Programación de Salidas: spro_programasalidas")
		RETURN FALSE
	ELSEIF sqlca.SQLCode	= 100 AND ii_lineatrans <> 0 THEN
		MessageBox("Error","No existe un Proceso activo en estos momentos.")
		RETURN FALSE
	END IF	

	ii_cliente2 = li_cliente

	SELECT	DISTINCT psrd_consol, psrd_lincon, cate_codigo
		INTO	:li_conso, :li_salidaconso, :ii_categoria
		FROM	dba.spro_programasalidadeta psd
		WHERE	( psd.plde_codigo 	= 	:gstr_paramplanta.codigoplanta ) 		and  
				( psd.orpr_tipord 	= 	:li_tipo ) 		and  
				( psd.orpr_numero 	= 	:ll_numero ) 	and  
				( psd.clie_codigo 	= 	:li_cliente ) 	and  
				( psd.lisa_codigo 	= 	:il_salidatrans )and
				( psd.line_codigo		=	:ii_lineatrans );

	IF sqlca.SQLCode	= -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Programación de Salidas: spro_programasalidas")
		RETURN FALSE
	ELSEIF sqlca.SQLCode	= 100 AND ii_lineatrans <> 0 THEN
		MessageBox("Error","No existe un Proceso activo en estos momentos.")
		RETURN FALSE
	END IF

	IF li_conso = 1 THEN ii_lineapack = li_salidaconso

	ii_proceso		=	ll_numero
	il_lotemenor	=	LoteProceso(li_tipo, ll_numero, li_cliente, gstr_paramplanta.codigoplanta)

	IF il_lotemenor = -1 THEN
		MessageBox("Error","No existen Lotes para el Proceso Activo.")
		RETURN FALSE
	END IF

	SELECT	Count(DISTINCT emba_codigo)
		INTO	:li_agrupa
		FROM	dba.spro_programasalidadeta psd
		WHERE	( psd.plde_codigo 		= 	:gstr_paramplanta.codigoplanta ) 		and  
						( psd.orpr_tipord = 	:li_tipo ) 		and  
						( psd.orpr_numero = 	:ll_numero ) 	and  
						( psd.clie_codigo = 	:li_cliente ) 	and  
						( psd.lisa_codigo = 	:il_salidatrans )and
						( psd.line_codigo	=	:ii_lineatrans );

	IF sqlca.SQLCode	= -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Programación de Salidas: spro_programasalidas")
		RETURN FALSE
	ELSEIF sqlca.SQLCode	= 100 AND ii_lineatrans <> 0 THEN
		MessageBox("Error","No existe un Proceso activo en estos momentos.")
		RETURN FALSE
	ELSEIF li_agrupa > 1 THEN
		ib_MultiEmbalaje	=	True
		ll_filas	=	dw_embala.Retrieve(gstr_paramplanta.codigoplanta, li_tipo, ll_numero, li_cliente, ii_linea, ii_lineapack )
		IF ll_filas < 1 AND ii_lineatrans <> 0 THEN
			MessageBox("Advertencia", "No Existen Embalajes para esta Salida en la Programación Activa.")
			RETURN FALSE
		END IF
	ELSE
		ib_MultiEmbalaje	=	FALSE
		SELECT	psd.emba_codigo, tpem_cancaj
			INTO	:is_embala, :ii_totalpallet
			FROM	dba.spro_programasalidadeta psd,
						dba.tipopallemba tpe
			WHERE	( psd.clie_codigo	=	tpe.clie_codigo )
			AND 	( psd.emba_codigo	=	tpe.emba_codigo )
			AND	( psd.tpem_codigo	=	tpe.tpem_codigo )
			AND	( psd.plde_codigo = 	:gstr_paramplanta.codigoplanta )
			AND	( psd.orpr_tipord = 	:li_tipo ) 
			AND	( psd.orpr_numero = 	:ll_numero )
			AND	( psd.clie_codigo = 	:li_cliente )
			AND 	( psd.lisa_codigo = 	:il_salidatrans )
			AND	( psd.line_codigo	=	:ii_lineatrans );		
	END IF

	SELECT Count(DISTINCT psd.prsd_calibr)
			INTO :li_agrupa
			FROM dba.spro_programasalidadeta psd
		WHERE	( psd.plde_codigo	= 	:gstr_paramplanta.codigoplanta )
		  AND ( psd.orpr_tipord = 	:li_tipo )
		  AND ( psd.orpr_numero = 	:ll_numero )
		  AND ( psd.clie_codigo = 	:li_cliente )
		  AND	( psd.lisa_codigo = 	:il_salidatrans )
		  AND	( psd.line_codigo	=	:ii_lineatrans );
	IF sqlca.SQLCode	= -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Programación de Salidas: spro_programasalidas")
		RETURN FALSE
	ELSEIF sqlca.SQLCode	= 100 AND ii_lineatrans <> 0 THEN
		MessageBox("Error","No existe un Proceso activo en estos momentos.")
		RETURN FALSE
	ELSEIF li_agrupa > 1 THEN
		ib_MultiCalibre	=	True
		ll_filas	=	dw_calibr.Retrieve(gstr_paramplanta.codigoplanta, li_tipo, ll_numero, li_cliente, ii_linea, ii_lineapack )
		IF ll_filas < 2 AND ii_lineatrans <> 0 THEN
			MessageBox("Advertencia", "No Existen Calibres para esta Salida en la Programación Activa.")
			RETURN FALSE
		END IF
	ELSE
		ib_MultiCalibre	=	FALSE
		SELECT psd.prsd_calibr
				INTO :is_calib
				FROM dba.spro_programasalidadeta psd
				WHERE	( psd.plde_codigo = 	:gstr_paramplanta.codigoplanta )
				AND	( psd.orpr_tipord = 	:li_tipo )
				AND	( psd.orpr_numero = 	:ll_numero )
				AND	( psd.clie_codigo = 	:li_cliente )
				AND 	( psd.lisa_codigo = 	:il_salidatrans )
				AND	( psd.line_codigo	=	:ii_lineatrans );
		IF LEN ( is_calib ) < 1 AND ii_lineatrans <> 0 THEN
			MessageBox("Advertencia", "No Existen Calibres para esta Salida en la Programación Activa.")
			RETURN FALSE
		END IF
	END IF
	
	RETURN TRUE
ELSE//******************************************************************************************************
	SELECT	loco_comlin, loco_nropal
		INTO	:li_lineapack, :il_loco_nropal
		FROM	dba.spro_correlcompequipo
		WHERE	plde_codigo 			= 	:gstr_paramplanta.codigoplanta	
		AND 	line_codigo 			=	:ii_linea
		AND 	Upper(equi_nombre)	= 	Upper(:is_Computador);
		
		IF isNull(ii_lineapack) OR ii_lineapack = 0 THEN ii_lineapack = li_lineapack
		
	IF sqlca.SQLCode	= -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de correlativos de compactos: spro_correlcompequipo")
		RETURN FALSE
	ELSEIF sqlca.SQLCode	= 100 THEN
		MessageBox("Error","No existe correlativos de Compactos para el Presente Equipo: " + is_computador)
		RETURN FALSE
	END IF
	
	
	SELECT	orpr_tipord, orpr_numero, clie_codigo, prsa_lineaa
		INTO	:li_tipo, :ll_numero, :li_cliente, :li_secuen
		FROM	dba.spro_programasalidas 
		WHERE	prsa_estado	= 	1
		AND	plde_codigo = 	:gstr_paramplanta.codigoplanta
		AND 	prsa_lineaa	=	:ii_linea;
		
	IF sqlca.SQLCode	= -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Programación de Salidas: spro_programasalidas")
		RETURN FALSE
	ELSEIF sqlca.SQLCode	= 100 THEN
		MessageBox("Error","No existe un Proceso activo en estos momentos.")
		RETURN FALSE
	END IF	
	
	ii_cliente2 = li_cliente
	
	SELECT	DISTINCT psrd_consol, psrd_lincon, cate_codigo
		INTO	:li_conso, :li_salidaconso, :ii_categoria
		FROM	dba.spro_programasalidadeta psd
		WHERE	( psd.plde_codigo 	= 	:gstr_paramplanta.codigoplanta ) 		and  
				( psd.orpr_tipord 	= 	:li_tipo ) 		and  
				( psd.orpr_numero 	= 	:ll_numero ) 	and  
				( psd.clie_codigo 	= 	:li_cliente ) 	and  
				( psd.lisa_codigo 	= 	:ii_lineapack )and
				( psd.line_codigo		=	:ii_linea );
						
	IF sqlca.SQLCode	= -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Programación de Salidas: spro_programasalidas")
		RETURN FALSE
	ELSEIF sqlca.SQLCode	= 100 THEN
		MessageBox("Error","No existe un Proceso activo en estos momentos.")
		RETURN FALSE
	END IF
	
	IF li_conso = 1 THEN ii_lineapack = li_salidaconso
	
	ii_proceso		=	ll_numero
	il_lotemenor	=	LoteProceso(li_tipo, ll_numero, li_cliente, gstr_paramplanta.codigoplanta)
	
	IF il_lotemenor = -1 THEN
		MessageBox("Error","No existen Lotes para el Proceso Activo.")
		RETURN FALSE
	END IF
	
	SELECT	Count(DISTINCT emba_codigo)
		INTO	:li_agrupa
		FROM	dba.spro_programasalidadeta psd
		WHERE	( psd.plde_codigo 		= 	:gstr_paramplanta.codigoplanta ) 		and  
						( psd.orpr_tipord = 	:li_tipo ) 		and  
						( psd.orpr_numero = 	:ll_numero ) 	and  
						( psd.clie_codigo = 	:li_cliente ) 	and  
						( psd.lisa_codigo = 	:ii_lineapack )and
						( psd.line_codigo =	:ii_linea );
						
	IF sqlca.SQLCode	= -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Programación de Salidas: spro_programasalidas")
		RETURN FALSE
	ELSEIF sqlca.SQLCode	= 100 THEN
		MessageBox("Error","No existe un Proceso activo en estos momentos.")
		RETURN FALSE
	ELSEIF li_agrupa > 1 THEN
		ib_MultiEmbalaje	=	True
		ll_filas	=	dw_embala.Retrieve(gstr_paramplanta.codigoplanta, li_tipo, ll_numero, li_cliente, ii_linea, ii_lineapack )
		IF ll_filas < 1 THEN
			MessageBox("Advertencia", "No Existen Embalajes para esta Salida en la Programación Activa.")
			RETURN FALSE
		END IF
	ELSE
		ib_MultiEmbalaje	=	FALSE
		SELECT	psd.emba_codigo, tpem_cancaj
			INTO	:is_embala, :ii_totalpallet
			FROM	dba.spro_programasalidadeta psd,
						dba.tipopallemba tpe
			WHERE	( psd.clie_codigo	=	tpe.clie_codigo )
			AND 	( psd.emba_codigo	=	tpe.emba_codigo )
			AND	( psd.tpem_codigo	=	tpe.tpem_codigo )
			AND	( psd.plde_codigo = 	:gstr_paramplanta.codigoplanta )
			AND	( psd.orpr_tipord = 	:li_tipo ) 
			AND	( psd.orpr_numero = 	:ll_numero )
			AND	( psd.clie_codigo = 	:li_cliente )
			AND	( psd.lisa_codigo = 	:ii_lineapack )
			AND	( psd.line_codigo	=	:ii_linea );
	END IF
	
	SELECT Count(DISTINCT psd.prsd_calibr)
			INTO :li_agrupa
			FROM dba.spro_programasalidadeta psd
		WHERE	( psd.plde_codigo	= 	:gstr_paramplanta.codigoplanta )
		  AND ( psd.orpr_tipord = 	:li_tipo )
		  AND ( psd.orpr_numero = 	:ll_numero )
		  AND ( psd.clie_codigo = 	:li_cliente )
		  AND ( psd.lisa_codigo = 	:ii_lineapack )
		  AND ( psd.line_codigo =	:ii_linea );
	IF sqlca.SQLCode	= -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Programación de Salidas: spro_programasalidas")
		RETURN FALSE
	ELSEIF sqlca.SQLCode	= 100 THEN
		MessageBox("Error","No existe un Proceso activo en estos momentos.")
		RETURN FALSE
	ELSEIF li_agrupa > 1 THEN
		ib_MultiCalibre	=	True
		ll_filas	=	dw_calibr.Retrieve(gstr_paramplanta.codigoplanta, li_tipo, ll_numero, li_cliente, ii_linea, ii_lineapack )
		IF ll_filas < 2 THEN
			MessageBox("Advertencia", "No Existen Calibres para esta Salida en la Programación Activa.")
			RETURN FALSE
		END IF
	ELSE
		ib_MultiCalibre	=	FALSE
		SELECT psd.prsd_calibr
				INTO :is_calib
				FROM dba.spro_programasalidadeta psd
				WHERE	( psd.plde_codigo = 	:gstr_paramplanta.codigoplanta )
				AND	( psd.orpr_tipord = 	:li_tipo )
				AND	( psd.orpr_numero = 	:ll_numero )
				AND	( psd.clie_codigo = 	:li_cliente )
				AND	( psd.lisa_codigo = 	:ii_lineapack )
				AND	( psd.line_codigo	=	:ii_linea );
		IF LEN ( is_calib ) < 1 THEN
			MessageBox("Advertencia", "No Existen Calibres para esta Salida en la Programación Activa.")
			RETURN FALSE
		END IF
	END IF
	
	RETURN TRUE
END IF
end function

public subroutine palletactual ();SELECT	loco_nropal
	INTO	:il_loco_nropal
	FROM	dba.spro_correlcompequipo
	WHERE	equi_nombre	= 	:is_Computador
	AND 		plde_codigo 	= 	:gstr_paramplanta.codigoplanta;
	
RETURN
end subroutine

public function boolean validaprocedimiento ();Boolean 	lb_respuesta = TRUE
Integer	li_estado

	select prsa_estado INTO :li_estado
        from dba.spro_programasalidas
        where plde_codigo = :gstr_paramplanta.codigoplanta
          and orpr_tipord = 4
          and orpr_numero = :ii_proceso
          and clie_codigo = :ii_Cliente;

IF sqlca.SQLCode	= -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura en la función de validación del proceso: validaprocedimiento")
END IF

IF IsNull(li_estado ) OR li_estado <> 1 THEN lb_respuesta = FALSE

RETURN lb_respuesta
end function

public function boolean validamultiprograma ();Integer	li_programaciones

SELECT count(*) INTO :li_programaciones
FROM	dba.spro_programasalidas 
WHERE	prsa_estado	= 1
AND	plde_codigo = 	:gstr_paramplanta.codigoplanta
AND 	prsa_lineaa = 	:ii_linea;

IF sqlca.SQLCode	= -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Programaciones de Salidas: spro_programasalidas ")
	RETURN FALSE
ELSEIF sqlca.SQLCode	= 100 OR li_programaciones = 0 OR IsNull(li_programaciones) THEN
	MessageBox("Error","No existen programas activos para la Linea\Planta.")
	RETURN FALSE
ELSEIF li_programaciones > 1 THEN
	MessageBox("Error","Existe más de un programa activo para la Linea\Planta.")
	RETURN FALSE
END IF

RETURN TRUE
end function

public function boolean cargaparametros ();Integer	li_control

select Min(line_codigo), Min(loco_comlin), count(*)
  into :ii_linea, :ii_salida, :li_control
  from dba.spro_correlcompequipo
 where equi_nombre = :is_Computador
   and plde_codigo =	:ii_Planta;
	

IF sqlca.SQLCode	= -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de parametros desde spro_correlcompequipo")
	RETURN FALSE
ELSEIF sqlca.SQLCode	= 100 OR li_control = 0 OR IsNull(li_control) THEN
	MessageBox("Error","El computador no se encuentra habilitado para la emision de compactos.")
	RETURN FALSE
ELSEIF li_control > 1 THEN
	MessageBox("Error","Existe más de un computador habilitado con el mismo nombre para esta planta.")
	RETURN FALSE
END IF

RETURN TRUE
end function

public subroutine existeprogramacion ();String	ls_prog
Integer	li_find

ls_prog	=	Right(sle_lectura.Text, Len(sle_lectura.Text) - 3)

li_find	=	dw_embala.Find("prsd_codsec = '" + ls_prog + "'", 1, dw_embala.RowCount())

IF li_find = 0 THEN
	sle_lectura.Text	=	"01NoRead&02NoRead&03NoRead&04NoRead"
ELSE
	sle_lectura.Text	=	"&01"+String(ii_proceso)+"&02"+String(il_lotemenor)+&
								"&03"+dw_embala.Object.emba_codigo[li_find]+&
								"&04"+dw_embala.Object.prsd_calibr[li_find]
END IF
end subroutine

on w_mant_mues_spro_cajasprod.create
this.sle_lectura=create sle_lectura
this.pb_grafico=create pb_grafico
this.st_advertencia=create st_advertencia
this.dw_5=create dw_5
this.dw_1=create dw_1
this.pb_ok=create pb_ok
this.pb_salir=create pb_salir
this.gb_1=create gb_1
this.gb_2=create gb_2
this.gb_4=create gb_4
this.gb_5=create gb_5
this.st_1=create st_1
this.dw_embala=create dw_embala
this.dw_calibr=create dw_calibr
this.rb_l1=create rb_l1
this.rb_l2=create rb_l2
this.gb_3=create gb_3
this.dw_4=create dw_4
this.dw_7=create dw_7
this.dw_2=create dw_2
this.Control[]={this.sle_lectura,&
this.pb_grafico,&
this.st_advertencia,&
this.dw_5,&
this.dw_1,&
this.pb_ok,&
this.pb_salir,&
this.gb_1,&
this.gb_2,&
this.gb_4,&
this.gb_5,&
this.st_1,&
this.dw_embala,&
this.dw_calibr,&
this.rb_l1,&
this.rb_l2,&
this.gb_3,&
this.dw_4,&
this.dw_7,&
this.dw_2}
end on

on w_mant_mues_spro_cajasprod.destroy
destroy(this.sle_lectura)
destroy(this.pb_grafico)
destroy(this.st_advertencia)
destroy(this.dw_5)
destroy(this.dw_1)
destroy(this.pb_ok)
destroy(this.pb_salir)
destroy(this.gb_1)
destroy(this.gb_2)
destroy(this.gb_4)
destroy(this.gb_5)
destroy(this.st_1)
destroy(this.dw_embala)
destroy(this.dw_calibr)
destroy(this.rb_l1)
destroy(this.rb_l2)
destroy(this.gb_3)
destroy(this.dw_4)
destroy(this.dw_7)
destroy(this.dw_2)
end on

event open;Long		ll_Fila

x					=	4
y					=	4
id_Fecha			=	Today()
ii_Cliente		=	100

iuo_especie		=	Create uo_especie

ii_Planta		=	gstr_ParamPlanta.CodigoPlanta
ii_especie		=	gstr_ParamPlanta.CodigoEspecie

iuo_especie.Existe(ii_especie, true, sqlca)

ib_Bloqueo		=	False

ii_linea 		= 	1

SetNull(il_NroCaja)

iuo_predio        		=  Create uo_predios  
iuo_embalajesprod 		=	Create uo_embalajesprod
iuo_buscadatosproceso	=	Create uo_buscadatosproceso	
iuo_correl					=	Create uo_lotescorrelequipo

//IF gs_Impresora = 'Zebra_2844' THEN
//	dw_2.DataObject="dw_info_spro_cajasprod_tlp2844z"	
//ELSEIF gs_Impresora = 'Zebra5200-200' THEN
//	dw_2.DataObject="dw_info_spro_cajasprod"
//ELSE	
//	dw_2.DataObject="dw_info_spro_cajasprod"
//	//dw_2.DataObject="dw_info_spro_cajasprod_tlp2844z"
	dw_2.DataObject="dw_info_spro_cajasprod_citricos"
//END IF

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_7.SetTransObject(sqlca)

dw_embala.SetTRansObject(sqlca)
dw_calibr.SetTRansObject(sqlca)

RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", &
				"ComputerName", RegString!, is_Computador)

IF NOT CargaParametros() THEN
	HALT
END IF

IF gb_programado THEN
	IF ValidaMultiPrograma() THEN
		ib_respuesta	=	CargaPrograma()
		IF NOT ib_respuesta THEN
			HALT
		END IF
		This.Height		=	1540
	ELSE
		HALT
	END IF
ELSE
	dw_embala.visible = 	False
	This.Height			=	1132
END IF

IF NOT BuscaNuevoCorrelativo(ii_Planta,ii_Cliente) THEN
	HALT
END IF

dw_1.InsertRow(0)
Show(This)
end event

event closequery;dw_1.accepttext()
IF dw_1.modifiedcount() > 0 THEN 
	CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
		CASE 1
			Message.DoubleParm = 0
			triggerevent("ue_guardar")
			IF message.doubleparm = -1 THEN Message.ReturnValue = 1
			RETURN 
		CASE 3
			Message.ReturnValue = 1
			RETURN
	END CHOOSE
END IF
end event

type sle_lectura from singlelineedit within w_mant_mues_spro_cajasprod
integer x = 105
integer y = 236
integer width = 2555
integer height = 136
integer taborder = 10
integer textsize = -16
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type pb_grafico from picturebutton within w_mant_mues_spro_cajasprod
boolean visible = false
integer x = 4407
integer y = 720
integer width = 155
integer height = 132
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "Custom067!"
alignment htextalign = left!
end type

event clicked;dw_4.Retrieve(ii_Cliente, id_Fecha, ii_Planta, 1)
dw_5.Retrieve(ii_Cliente, id_Fecha, ii_Planta ,2)
//dw_6.Retrieve(ii_Cliente, id_Fecha, ii_Planta, 3)
dw_7.Retrieve(ii_Cliente, id_Fecha, ii_Planta, 4)
dw_1.SetColumn("capr_regcap")
dw_1.SetFocus()
end event

type st_advertencia from statictext within w_mant_mues_spro_cajasprod
integer x = 910
integer y = 36
integer width = 942
integer height = 116
integer textsize = -20
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 8388608
string text = "EN PROCESO"
alignment alignment = center!
boolean focusrectangle = false
end type

type dw_5 from datawindow within w_mant_mues_spro_cajasprod
boolean visible = false
integer x = 27
integer y = 2220
integer width = 1147
integer height = 988
boolean enabled = false
boolean titlebar = true
string title = "Distribución Embalaje"
string dataobject = "dw_info_graficos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_1 from datawindow within w_mant_mues_spro_cajasprod
integer x = 32
integer y = 448
integer width = 2693
integer height = 568
string title = "Caja Anterior"
string dataobject = "dw_mant_mues_spro_cajasprod"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type pb_ok from picturebutton within w_mant_mues_spro_cajasprod
event ue_mousemove pbm_mousemove
integer x = 2821
integer y = 524
integer width = 155
integer height = 132
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo\Bmp\ACEPTAE.BMP"
alignment htextalign = left!
end type

event clicked;String	ls_Registro, ls_Null
Integer	li_Etiqueta, li_retorno, li_error, li_error01, li_error02, li_error03, &
			li_error04, li_error05, li_clietrans

SetNull(ls_Null)

IF ib_Bloqueo THEN
	Return
ELSE
	ib_Bloqueo	=	TRUE
END IF

li_error = Pos(sle_lectura.Text,"NOREAD")
li_error01 = Pos(sle_lectura.Text,"01")
li_error02 = Pos(sle_lectura.Text,"02")
li_error03 = Pos(sle_lectura.Text,"03")
li_error04 = Pos(sle_lectura.Text,"04")
li_error05 = Pos(sle_lectura.Text,"05")

IF IsNull(sle_lectura.Text) OR &
	sle_lectura.Text = "" OR &
	sle_lectura.Text = "-1" THEN	
	sle_lectura.Text = ls_Null
	sle_lectura.SetFocus()
ELSE
	IF li_error05 > 0 THEN
		ExisteProgramacion()
	END IF
	
	ls_Registro	=	sle_lectura.Text
	
	dw_1.Reset()
	dw_2.Reset()
	dw_1.InsertRow(0)
	IF (li_error = 0 AND li_error01 <> 0 AND li_error02 <> 0 AND li_error03 <> 0 AND li_error04 <> 0) OR li_error05 <> 0 THEN
		IF DespieceRegistro(ls_Registro) = TRUE  THEN
			IF BuscaDatosProceso(ii_proceso) = TRUE THEN
				
				PalletActual()
				
				dw_1.Object.capr_numero[1]		=	il_NroCaja
				dw_1.Object.capr_numpal[1]		=	il_loco_nropal
				dw_1.Object.capr_regcap[1]		=	ls_Registro
				dw_1.Object.capr_pcline[1]		=	is_Computador
				dw_1.Object.capr_numgia[1]		=	ii_totalpallet
				dw_1.Object.capr_lineas[1]		=	ii_lineapack
				dw_1.Object.capr_docrel[1]		=	ii_proceso
				dw_1.Object.etiq_codigo[1]		=	li_Etiqueta
				
				il_NroCaja++
				
				IF IsNull(dw_1.Object.emba_codigo[1]) THEN 	
					dw_1.Object.emba_codigo[1] =	is_embala
				END IF
				
				IF IsNull(dw_1.Object.capr_calibr[1]) THEN 	
					dw_1.Object.capr_calibr[1]	=		Left(Trim(is_calib) + '   ',3)
				END IF
				
				IF IsNull(dw_1.Object.capr_nrlote[1]) THEN 	
					dw_1.Object.capr_nrlote[1] =  il_lotemenor
				END IF
				
				IF IsNull(dw_1.Object.capr_lineas[1]) OR dw_1.Object.capr_lineas[1] = 0 THEN 
					dw_1.Object.capr_lineas[1] = iuo_correl.loco_comlin
				END IF
				
				IF IsNull(iuo_buscadatosproceso.Etiqueta) THEN 
					li_Etiqueta	=  1
				ELSE
					li_Etiqueta	=	iuo_buscadatosproceso.Etiqueta
				END IF
				
				IF gstr_paramplanta.GenPucho = 1 THEN	dw_1.Object.capr_estado[1]	=	1
				
				IF dw_1.Object.clie_codigo[1] < 100 THEN
					li_clietrans = dw_1.Object.clie_codigo[1] * 1000000+ &
										dw_1.Object.plde_codigo[1] * 10000 + &
										dw_1.Object.capr_docrel[1]
				ELSE
					li_clietrans = dw_1.Object.clie_codigo[1] * 100000+ &
										dw_1.Object.plde_codigo[1] * 1000 + &
										dw_1.Object.capr_docrel[1]
				END IF
				dw_1.Object.capr_numtra[1]	=	li_clietrans 
				
				dw_1.AcceptText()
				
				Parent.TriggerEvent("ue_guardar")
				Parent.TriggerEvent("ue_imprimir")
			ELSE
				Parent.TriggerEvent("ue_imprimirerror")
			END IF
		ELSE
			Parent.TriggerEvent("ue_imprimirerror")
		END IF
	ELSE
		Parent.TriggerEvent("ue_imprimirerror")
	END IF
	
	sle_lectura.Text = ls_Null
	sle_lectura.SetFocus()
END IF

ib_Bloqueo	=	False
end event

type pb_salir from picturebutton within w_mant_mues_spro_cajasprod
event ue_mousemove pbm_mousemove
integer x = 2821
integer y = 848
integer width = 155
integer height = 132
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\exit.bmp"
alignment htextalign = left!
end type

event ue_mousemove;
end event

event clicked;Close(Parent)
end event

type gb_1 from groupbox within w_mant_mues_spro_cajasprod
boolean visible = false
integer x = 4357
integer y = 644
integer width = 251
integer height = 248
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_2 from groupbox within w_mant_mues_spro_cajasprod
integer x = 2775
integer y = 452
integer width = 251
integer height = 248
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_4 from groupbox within w_mant_mues_spro_cajasprod
integer x = 2775
integer y = 772
integer width = 251
integer height = 248
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_5 from groupbox within w_mant_mues_spro_cajasprod
integer x = 73
integer y = 188
integer width = 2615
integer height = 204
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type st_1 from statictext within w_mant_mues_spro_cajasprod
integer x = 32
integer y = 180
integer width = 2693
integer height = 252
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

type dw_embala from datawindow within w_mant_mues_spro_cajasprod
integer x = 453
integer y = 1032
integer width = 1847
integer height = 400
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_embalajes_etiq_salidas"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_calibr from datawindow within w_mant_mues_spro_cajasprod
integer x = 32
integer y = 1448
integer width = 2693
integer height = 400
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_calibres_etiq_salidas"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type rb_l1 from radiobutton within w_mant_mues_spro_cajasprod
boolean visible = false
integer x = 750
integer y = 2592
integer width = 325
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Uno"
boolean checked = true
end type

event clicked;ii_linea = 1
SetNull(il_NroCaja)
IF BuscaNuevoCorrelativo(ii_Planta,ii_Cliente) = True THEN
END IF
end event

type rb_l2 from radiobutton within w_mant_mues_spro_cajasprod
boolean visible = false
integer x = 750
integer y = 2684
integer width = 325
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Dos"
end type

event clicked;ii_linea = 2
SetNull(il_NroCaja)
IF BuscaNuevoCorrelativo(ii_Planta,ii_Cliente) = True THEN
END IF
end event

type gb_3 from groupbox within w_mant_mues_spro_cajasprod
boolean visible = false
integer x = 672
integer y = 2516
integer width = 480
integer height = 308
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Línea"
end type

type dw_4 from datawindow within w_mant_mues_spro_cajasprod
boolean visible = false
integer x = 23
integer y = 2208
integer width = 1147
integer height = 988
boolean enabled = false
boolean titlebar = true
string title = "Distribución Calibre"
string dataobject = "dw_info_graficos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_7 from datawindow within w_mant_mues_spro_cajasprod
boolean visible = false
integer x = 1184
integer y = 2204
integer width = 3424
integer height = 1988
boolean enabled = false
boolean titlebar = true
string title = "Kilos Por Procesos"
string dataobject = "dw_grafico_kilos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_2 from datawindow within w_mant_mues_spro_cajasprod
boolean visible = false
integer x = 942
integer y = 2284
integer width = 1079
integer height = 556
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_spro_cajasprod_citricos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

