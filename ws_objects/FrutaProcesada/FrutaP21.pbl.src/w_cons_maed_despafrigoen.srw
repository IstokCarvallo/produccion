$PBExportHeader$w_cons_maed_despafrigoen.srw
forward
global type w_cons_maed_despafrigoen from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_cons_maed_despafrigoen
end type
type dw_4 from datawindow within w_cons_maed_despafrigoen
end type
type dw_5 from datawindow within w_cons_maed_despafrigoen
end type
type dw_6 from datawindow within w_cons_maed_despafrigoen
end type
type dw_7 from datawindow within w_cons_maed_despafrigoen
end type
type dw_8 from datawindow within w_cons_maed_despafrigoen
end type
type dw_9 from datawindow within w_cons_maed_despafrigoen
end type
type dw_10 from datawindow within w_cons_maed_despafrigoen
end type
type dw_11 from datawindow within w_cons_maed_despafrigoen
end type
type dw_12 from datawindow within w_cons_maed_despafrigoen
end type
type dw_13 from datawindow within w_cons_maed_despafrigoen
end type
type dw_14 from datawindow within w_cons_maed_despafrigoen
end type
type dw_15 from datawindow within w_cons_maed_despafrigoen
end type
type dw_alpalletencab from datawindow within w_cons_maed_despafrigoen
end type
type dw_alpalletfruta from datawindow within w_cons_maed_despafrigoen
end type
type dw_palletencahisto from datawindow within w_cons_maed_despafrigoen
end type
type dw_palletfrutahisto from datawindow within w_cons_maed_despafrigoen
end type
end forward

global type w_cons_maed_despafrigoen from w_mant_encab_deta_csd
integer width = 3429
integer height = 2372
string title = "DESPACHO DE FRUTA PROCESADA"
string menuname = ""
boolean maxbox = false
event ue_imprimir ( )
event ue_despuesguardar ( )
event ue_despuesgrabar ( )
dw_3 dw_3
dw_4 dw_4
dw_5 dw_5
dw_6 dw_6
dw_7 dw_7
dw_8 dw_8
dw_9 dw_9
dw_10 dw_10
dw_11 dw_11
dw_12 dw_12
dw_13 dw_13
dw_14 dw_14
dw_15 dw_15
dw_alpalletencab dw_alpalletencab
dw_alpalletfruta dw_alpalletfruta
dw_palletencahisto dw_palletencahisto
dw_palletfrutahisto dw_palletfrutahisto
end type
global w_cons_maed_despafrigoen w_cons_maed_despafrigoen

type variables
w_mant_deta_despafrigode iw_mantencion

DataWindowChild	dw_planta, dw_plades, dw_puerto,dw_etiquetas,dw_plantas,dw_clientes,&
						dw_ptodes,dw_sitios,idwc_patente

Transaction	sqlconec
Transaction	sqlconec2

Boolean		ib_conectado, ib_existe_folioD, ib_conectado2
Integer     ii_tipoin, ii_secuen, ii_controlaaceso 
Long        il_numins, il_Folio

DataStore	ids_palletfruta_fecha, ids_CorrelMovim

uo_patente				iuo_patente
uo_calibre				iuo_calibre
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso (string columna)
public function boolean noexisteembarque (string as_embarque)
public subroutine buscaembarque ()
public function boolean conexionbase ()
public function long buscanuevofolio (integer planta)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean wf_actualiza_carga (boolean borrando)
public subroutine carga_inspeccion (long pallet)
public function boolean busca_numeroinspeccion (integer cliente, integer planta, long pallet)
public subroutine existe_cargaregistro ()
public subroutine generaarchivoplano ()
public function long busnuevofoliodespa (integer ai_cliente, integer ai_planta)
public function boolean existefolio (string as_columna, string as_valor)
public function boolean buscaregistros ()
public function boolean coneccionbase ()
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "DESPACHO DE FRUTA PROCESADA"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_despafrigoen"

vinf.dw_1.GetChild("plde_codigo", dw_plantas)
vinf.dw_1.GetChild("etiq_codigo", dw_etiquetas)
vinf.dw_1.GetChild("clie_codigo", dw_clientes)

dw_plantas.SetTransObject(sqlca)
dw_etiquetas.SetTransObject(sqlca)
dw_clientes.SetTransObject(sqlca)

dw_plantas.Retrieve(1)
dw_etiquetas.Retrieve()
dw_clientes.Retrieve()

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]),Long(istr_mant.argumento[2]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

event ue_despuesguardar();//
Long ll_fila, ll_Pfruta, ll_numins, ll_fila2

SetPointer(HourGlass!)

IF ConexionBase() THEN
	
	IF buscaregistros() THEN
//		w_mensaje.st_1.text = " Registro Existe En Base de Datos"
//		Close(w_mensaje)
		Message.DoubleParm = -1
		Return 
	END IF	
	
	Open(w_mensaje)
	w_mensaje.st_1.text = " Transmisión Automática en Curso"
	
	dw_3.SetTransObject(sqlconec)
	dw_4.SetTransObject(sqlconec)
	dw_7.SetTransObject(sqlconec)
	dw_8.SetTransObject(sqlconec)
	dw_11.SetTransObject(sqlconec)
	dw_12.SetTransObject(sqlconec)
	dw_15.SetTransObject(sqlconec)
	
   dw_3.GetChild("plde_codigo", dw_planta)
   dw_planta.SetTransObject(sqlconec)
	dw_planta.Retrieve(1)
	dw_3.GetChild("defe_plades", dw_plades)
	dw_plades.SetTransObject(sqlconec)
	dw_plades.Retrieve(1)
	dw_3.GetChild("puer_codigo", dw_puerto)
   dw_puerto.SetTransObject(sqlconec)
   dw_puerto.Retrieve(1)
	
	dw_3.Insertrow(0)
	dw_3.Object.plde_codigo[1]  = dw_2.Object.defe_plades[1]
	dw_3.Object.rfpe_ptaori[1]  = dw_2.Object.plde_codigo[1]
   dw_3.Object.rfpe_numero[1]  = dw_2.Object.defe_numero[1]
	//dw_3.Object.rfpe_numero[1]  = BuscaNuevoFolio(dw_2.Object.defe_plades[1])
	dw_3.Object.clie_codigo[1]  = dw_2.Object.clie_codigo[1]
	dw_3.Object.rfpe_fecrec[1]  = Today()
	dw_3.Object.rfpe_tipoen[1]  = 2
	dw_3.Object.rfpe_tarjas[1]  = dw_2.Object.defe_cantar[1]
	dw_3.Object.rfpe_guides[1]  = dw_2.Object.defe_guides[1]
	dw_3.Object.rfpe_nrores[1]  = dw_2.Object.defe_guides[1]
	dw_3.Object.tran_codigo[1]  = dw_2.Object.tran_codigo[1]
	dw_3.Object.tica_codigo[1]  = dw_2.Object.tica_codigo[1]
	dw_3.Object.rfpe_patent[1]  = dw_2.Object.defe_patent[1]
	dw_3.Object.tica_codigo[1]  = dw_2.Object.tica_codigo[1]
	dw_3.Object.rfpe_chofer[1]  = dw_2.Object.defe_chofer[1]
	dw_3.Object.rfpe_fecact[1]  = dw_2.Object.defe_fecact[1]
	dw_3.Object.rfpe_horact[1]  = dw_2.Object.defe_horact[1]
	dw_3.Object.rfpe_fecing[1]	 = dw_2.Object.defe_fecing[1]
	
	dw_5.GetChild("plde_codigo", dw_planta)
	dw_planta.SetTransObject(sqlca)
	dw_planta.Retrieve(1)
	
	IF dw_1.Rowcount() > 0 THEN
		
		FOR ll_Fila = 1 TO dw_1.Rowcount()
			dw_4.Insertrow(0)
			dw_4.Object.plde_codigo[ll_Fila] = dw_2.Object.defe_plades[1]
			dw_4.Object.rfpe_numero[ll_fila] = dw_2.Object.defe_numero[1]
			dw_4.Object.clie_codigo[ll_fila] = dw_2.Object.clie_codigo[1]
			dw_4.Object.paen_numero[ll_fila] = dw_1.Object.paen_numero[ll_fila]
			
			dw_5.Retrieve(dw_2.Object.clie_codigo[1],dw_1.Object.paen_numero[ll_fila],dw_2.Object.plde_codigo[1])
		   IF dw_5.RowCount() > 0 THEN
				//dw_5.RowsCopy(1,dw_5.RowCount(), Primary!, dw_7, 1, Primary!)
				dw_7.Object.paen_estado[ll_fila] = 1
		      dw_7.Object.plde_codigo[ll_fila] = dw_2.Object.defe_plades[1]
				dw_7.Object.clie_codigo[ll_fila]	= dw_5.Object.clie_codigo[1]
				dw_7.Object.paen_numero[ll_fila] = dw_5.Object.paen_numero[1]   
				dw_7.Object.paen_tipopa[ll_fila] = dw_5.Object.paen_tipopa[1]   
				dw_7.Object.espe_codigo[ll_fila] = dw_5.Object.espe_codigo[1]   
				dw_7.Object.vari_codigo[ll_fila] = dw_5.Object.vari_codigo[1]   
				dw_7.Object.tiem_codigo[ll_fila] = dw_5.Object.tiem_codigo[1]   
				dw_7.Object.emba_codigo[ll_fila] = dw_5.Object.emba_codigo[1] 
				dw_7.Object.cate_codigo[ll_fila] = dw_5.Object.cate_codigo[1]   
				dw_7.Object.etiq_codigo[ll_fila] = dw_5.Object.etiq_codigo[1]   
				dw_7.Object.stat_codigo[ll_fila] = dw_5.Object.stat_codigo[1]   
				dw_7.Object.trat_codigo[ll_fila] = dw_5.Object.trat_codigo[1]   
				dw_7.Object.frio_codigo[ll_fila] = dw_5.Object.frio_codigo[1]   
				dw_7.Object.cond_codigo[ll_fila] = dw_5.Object.cond_codigo[1]   
			//	dw_7.Object.plde_codigo[1] = dw_5.Object.plde_codigo[1]   
				dw_7.Object.paen_cosecha[ll_fila]= dw_5.Object.paen_cosecha[1]    
				dw_7.Object.paen_altura[ll_fila] = dw_5.Object.paen_altura[1]   
				dw_7.Object.tmvp_codigo[ll_fila] = dw_5.Object.tmvp_codigo[1]   
				dw_7.Object.paen_fecini[ll_fila] = dw_5.Object.paen_fecini[1]   
				dw_7.Object.paen_horain[ll_fila] = dw_5.Object.paen_horain[1]   
				dw_7.Object.cama_codigo[ll_fila] = dw_5.Object.cama_codigo[1]   
				dw_7.Object.paen_calle[ll_fila]  = dw_5.Object.paen_calle[1]   
				dw_7.Object.paen_base[ll_fila]   = dw_5.Object.paen_base[1] 
				dw_7.Object.paen_posici[ll_fila] = dw_5.Object.paen_posici[1]   
				dw_7.Object.paen_estado[ll_fila] = dw_5.Object.paen_estado[1]   
				dw_7.Object.paen_fecemb[ll_fila] = dw_5.Object.paen_fecemb[1]   
				dw_7.Object.vari_nombre[ll_fila] = dw_5.Object.vari_nombre[1]   
				dw_7.Object.emba_nombre[ll_fila] = dw_5.Object.emba_nombre[1]   
				dw_7.Object.tpem_codigo[ll_fila] = dw_5.Object.tpem_codigo[1]   
				dw_7.Object.paen_ccajas[ll_fila] = dw_5.Object.paen_ccajas[1]   
				dw_7.Object.paen_concal[ll_fila] = dw_5.Object.paen_concal[1]   
				dw_7.Object.paen_inspec[ll_fila] = dw_5.Object.paen_inspec[1]   
				dw_7.Object.dest_codigo[ll_fila] = dw_5.Object.dest_codigo[1]   
				dw_7.Object.paen_pexpor[ll_fila] = dw_5.Object.paen_pexpor[1]   
				dw_7.Object.paen_pmixto[ll_fila] = dw_5.Object.paen_pmixto[1]   
				dw_7.Object.paen_varrot[ll_fila] = dw_5.Object.paen_varrot[1]   
				dw_7.Object.paen_nrasda[ll_fila] = dw_5.Object.paen_nrasda[1]   
				dw_7.Object.copa_codigo[ll_fila] = dw_5.Object.copa_codigo[1]         
			END IF
			dw_6.Retrieve(dw_2.Object.clie_codigo[1],dw_1.Object.paen_numero[ll_fila],dw_2.Object.plde_codigo[1])
			IF dw_6.RowCount() > 0 THEN
				//dw_6.RowsCopy(1,dw_6.RowCount(), Primary!, dw_8, 1, Primary!)
				FOR ll_Pfruta = 1 TO dw_6.RowCount()
					ll_fila2	=	dw_8.InsertRow(0)
					dw_8.Object.plde_codigo[ll_fila2] = dw_2.Object.defe_plades[1]
					dw_8.Object.clie_codigo[ll_fila2] = dw_6.Object.clie_codigo[ll_Pfruta]
					dw_8.Object.paen_numero[ll_fila2] = dw_6.Object.paen_numero[ll_Pfruta]
					dw_8.Object.espe_codigo[ll_fila2] = dw_6.Object.espe_codigo[ll_Pfruta]
					dw_8.Object.vari_codigo[ll_fila2] = dw_6.Object.vari_codigo[ll_Pfruta]
					dw_8.Object.emba_codigo[ll_fila2] = dw_6.Object.emba_codigo[ll_Pfruta]
					dw_8.Object.prod_codigo[ll_fila2] = dw_6.Object.prod_codigo[ll_Pfruta]
					dw_8.Object.cond_codigo[ll_fila2] = dw_6.Object.cond_codigo[ll_Pfruta]
					dw_8.Object.etiq_codigo[ll_fila2] = dw_6.Object.etiq_codigo[ll_Pfruta]
					//dw_8.Object.plde_codigo[ll_Pfruta] = dw_6.Object.plde_codigo[ll_fila2]
					dw_8.Object.pafr_calibr[ll_fila2] = dw_6.Object.pafr_calibr[ll_Pfruta]
					dw_8.Object.pafr_secuen[ll_fila2] = dw_6.Object.pafr_secuen[ll_Pfruta]
					dw_8.Object.pafr_ccajas[ll_fila2] = dw_6.Object.pafr_ccajas[ll_Pfruta]
					dw_8.Object.pafr_nrlote[ll_fila2] = dw_6.Object.pafr_nrlote[ll_Pfruta]
					//dw_8.Object.vari_nombre[ll_Pfruta] = dw_6.Object.vari_nombre[ll_fila2]
					//dw_8.Object.emba_nombre[ll_Pfruta] = dw_6.Object.emba_nombre[ll_fila2]
					//dw_8.Object.prod_nombre[ll_Pfruta] = dw_6.Object.prod_nombre[ll_fila2]
					//dw_8.Object.clie_nombre[ll_Pfruta] = dw_6.Object.clie_nombre[ll_fila2]
					dw_8.Object.pafr_copack[ll_fila2] = dw_6.Object.pafr_copack[ll_Pfruta] //dw_6.Object.plde_codigo[ll_Pfruta]
					dw_8.Object.pafr_varrot[ll_fila2] = dw_6.Object.pafr_varrot[ll_Pfruta]
					dw_8.Object.pafr_prdrot[ll_fila2] = dw_6.Object.pafr_prdrot[ll_Pfruta]
					dw_8.Object.pafr_calrot[ll_fila2] = dw_6.Object.pafr_calrot[ll_Pfruta]
					dw_8.Object.pafr_huert1[ll_fila2] = dw_6.Object.pafr_huert1[ll_Pfruta]
         		dw_8.Object.pafr_cuart1[ll_fila2] = dw_6.Object.pafr_cuart1[ll_Pfruta]
					dw_8.Object.pafr_fecemb[ll_fila2] = dw_6.Object.pafr_fecemb[ll_Pfruta]
					dw_8.Object.pafr_fecing[ll_fila2] = dw_6.Object.pafr_fecing[ll_Pfruta]
					
				NEXT
			END IF
			
			dw_14.Retrieve(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_1.Object.paen_numero[ll_fila])
			IF dw_14.RowCount() > 0 THEN
				FOR ll_Pfruta = 1 TO dw_14.RowCount()
					ll_Fila2	=	dw_15.InsertRow(0)
					dw_15.Object.clie_codigo[ll_Fila2]	=	dw_14.Object.clie_codigo[ll_Pfruta]					
					dw_15.Object.plde_codigo[ll_Fila2]	=	dw_2.Object.defe_plades[1]
					dw_15.Object.capr_numero[ll_Fila2]	=	dw_14.Object.capr_numero[ll_Pfruta]	
					dw_15.Object.espe_codigo[ll_Fila2]	=	dw_14.Object.espe_codigo[ll_Pfruta]	
					dw_15.Object.vari_codigo[ll_Fila2]	=	dw_14.Object.vari_codigo[ll_Pfruta]						
					dw_15.Object.prod_codigo[ll_Fila2]	=	dw_14.Object.prod_codigo[ll_Pfruta]						
					dw_15.Object.prod_predio[ll_Fila2]	=	dw_14.Object.prod_predio[ll_Pfruta]	
					dw_15.Object.prod_huerto[ll_Fila2]	=	dw_14.Object.prod_huerto[ll_Pfruta]						
					dw_15.Object.prod_cuarte[ll_Fila2]	=	dw_14.Object.prod_cuarte[ll_Pfruta]	
					dw_15.Object.emba_codigo[ll_Fila2]	=	dw_14.Object.emba_codigo[ll_Pfruta]						
					dw_15.Object.etiq_codigo[ll_Fila2]	=	dw_14.Object.etiq_codigo[ll_Pfruta]						
					dw_15.Object.capr_fecemb[ll_Fila2]	=	dw_14.Object.capr_fecemb[ll_Pfruta]	
					dw_15.Object.capr_calibr[ll_Fila2]	=	dw_14.Object.capr_calibr[ll_Pfruta]	
					dw_15.Object.capr_embala[ll_Fila2]	=	dw_14.Object.capr_embala[ll_Pfruta]						
					dw_15.Object.capr_selecc[ll_Fila2]	=	dw_14.Object.capr_selecc[ll_Pfruta]	
					dw_15.Object.capr_pesado[ll_Fila2]	=	dw_14.Object.capr_pesado[ll_Pfruta]
					dw_15.Object.capr_cean14[ll_Fila2]	=	dw_14.Object.capr_cean14[ll_Pfruta]	
					dw_15.Object.capr_numpal[ll_Fila2]	=	dw_14.Object.capr_numpal[ll_Pfruta]	
					dw_15.Object.capr_regcap[ll_Fila2]	=	dw_14.Object.capr_regcap[ll_Pfruta]
					dw_15.Object.capr_estado[ll_Fila2]	=	dw_14.Object.capr_estado[ll_Pfruta]	
					dw_15.Object.capr_varrot[ll_Fila2]	=	dw_14.Object.capr_varrot[ll_Pfruta]	
					dw_15.Object.capr_numgia[ll_Fila2]	=	dw_14.Object.capr_numgia[ll_Pfruta]
					dw_15.Object.cate_codigo[ll_Fila2]	=	dw_14.Object.cate_codigo[ll_Pfruta]	
					dw_15.Object.capr_cespak[ll_Fila2]	=	dw_14.Object.capr_cespak[ll_Pfruta]	
					dw_15.Object.capr_docrel[ll_Fila2]	=	dw_14.Object.capr_docrel[ll_Pfruta]	
					dw_15.Object.capr_hordig[ll_Fila2]	=	dw_14.Object.capr_hordig[ll_Pfruta]	
					dw_15.Object.capr_fecdig[ll_Fila2]	=	dw_14.Object.capr_fecdig[ll_Pfruta]	
					dw_15.Object.capr_nrlote[ll_Fila2]	=	dw_14.Object.capr_nrlote[ll_Pfruta]						
				NEXT
			END IF 	
			
			IF dw_1.Object.paen_inspec[ll_fila] = 1 THEN
				IF Busca_Numeroinspeccion(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_1.Object.paen_numero[ll_fila]) THEN
					carga_inspeccion(dw_1.Object.paen_numero[ll_fila])
				END IF
			END IF						
			dw_6.Reset()
			dw_5.Reset()
			dw_14.Reset()		 
      NEXT		
	END IF
	Close(w_mensaje)	
END IF


end event

event ue_despuesgrabar();Long ll_fila2, ll_nueva2, ll_numero, ll_nueva, ll_fila1, ll_pallet,&
		ll_nueva1, ll_nueva3, ll_fila3, ll_despacho
Integer	li_cliente, li_planta
Boolean	lb_AutoCommit, lb_Retorno

SetPointer(HourGlass!)

li_cliente 	= dw_2.Object.clie_codigo[1]
li_planta 	= dw_2.Object.plde_codigo[1]
ll_despacho = dw_2.Object.defe_numero[1]

Select max(altu_numero) + 1
into :ll_numero
from dba.alpalletencab
where clie_codigo = :li_cliente
and	plde_codigo = :li_planta
and   altu_numero < 99999999;

ll_nueva = dw_alpalletencab.InsertRow(0)

dw_alpalletencab.Object.clie_codigo[ll_nueva] = li_cliente  
dw_alpalletencab.Object.plde_codigo[ll_nueva] = li_planta
dw_alpalletencab.Object.altu_numero[ll_nueva] = ll_numero
dw_alpalletencab.Object.altu_fecmov[ll_nueva] = dw_2.Object.defe_fecdes[1]
dw_alpalletencab.Object.altu_observ[ll_nueva] = 'Despacho'

FOR ll_fila1 = 1 TO dw_1.RowCount() 
	ll_pallet = dw_1.Object.paen_numero[ll_fila1]

	ll_nueva1 = dw_alpalletfruta.InsertRow(0)
	
	dw_alpalletfruta.Object.clie_codigo[ll_nueva1] = li_cliente  
	dw_alpalletfruta.Object.plde_codigo[ll_nueva1] = li_planta
	dw_alpalletfruta.Object.altu_numero[ll_nueva1] = ll_numero
	dw_alpalletfruta.Object.alpf_fecmov[ll_nueva1] = dw_2.Object.defe_fecdes[1]
	dw_alpalletfruta.Object.paen_numero[ll_nueva1] = ll_pallet
	
	dw_5.Retrieve(li_cliente,ll_pallet,li_planta)
	dw_6.Retrieve(li_cliente,ll_pallet,li_planta)
	
	FOR ll_fila2 = 1 TO dw_5.RowCount()
		ll_nueva2 = dw_palletencahisto.InsertRow(0)
	
		dw_palletencahisto.Object.clie_codigo[ll_nueva2] = li_cliente   
		dw_palletencahisto.Object.paen_numero[ll_nueva2] = dw_5.Object.paen_numero[ll_fila2]  
		dw_palletencahisto.Object.plde_codigo[ll_nueva2] = li_planta  
		dw_palletencahisto.Object.pahi_proces[ll_nueva2] = ll_numero  
		dw_palletencahisto.Object.pahi_tipopa[ll_nueva2] = dw_5.Object.paen_tipopa[ll_fila2]  
		dw_palletencahisto.Object.tpem_codigo[ll_nueva2] = dw_5.Object.tpem_codigo[ll_fila2]     
		dw_palletencahisto.Object.espe_codigo[ll_nueva2] = dw_5.Object.espe_codigo[ll_fila2]    
		dw_palletencahisto.Object.vari_codigo[ll_nueva2] = dw_5.Object.vari_codigo[ll_fila2]    
		dw_palletencahisto.Object.tiem_codigo[ll_nueva2] = dw_5.Object.tiem_codigo[ll_fila2]    
		dw_palletencahisto.Object.emba_codigo[ll_nueva2] = dw_5.Object.emba_codigo[ll_fila2]    
		dw_palletencahisto.Object.cate_codigo[ll_nueva2] = dw_5.Object.cate_codigo[ll_fila2]    
		dw_palletencahisto.Object.etiq_codigo[ll_nueva2] = dw_5.Object.etiq_codigo[ll_fila2]    
		dw_palletencahisto.Object.stat_codigo[ll_nueva2] = dw_5.Object.stat_codigo[ll_fila2]    
		dw_palletencahisto.Object.trat_codigo[ll_nueva2] = dw_5.Object.trat_codigo[ll_fila2]    
		dw_palletencahisto.Object.frio_codigo[ll_nueva2] = dw_5.Object.frio_codigo[ll_fila2]    
		dw_palletencahisto.Object.cond_codigo[ll_nueva2] = dw_5.Object.cond_codigo[ll_fila2]    
		dw_palletencahisto.Object.dest_codigo[ll_nueva2] = dw_5.Object.dest_codigo[ll_fila2]    
		dw_palletencahisto.Object.pahi_fecemb[ll_nueva2] = dw_5.Object.paen_fecemb[ll_fila2]    
		dw_palletencahisto.Object.pahi_cosecha[ll_nueva2] = dw_5.Object.paen_cosecha[ll_fila2]     
		dw_palletencahisto.Object.paen_altura[ll_nueva2] = dw_5.Object.paen_altura[ll_fila2]    
		dw_palletencahisto.Object.paen_ccajas[ll_nueva2] = dw_5.Object.paen_ccajas[ll_fila2]    
		dw_palletencahisto.Object.tmvp_codigo[ll_nueva2] = dw_5.Object.tmvp_codigo[ll_fila2]    
		dw_palletencahisto.Object.paen_fecini[ll_nueva2] = dw_5.Object.paen_fecini[ll_fila2]    
		dw_palletencahisto.Object.paen_horain[ll_nueva2] = dw_5.Object.paen_horain[ll_fila2]    
		dw_palletencahisto.Object.cama_codigo[ll_nueva2] = dw_5.Object.cama_codigo[ll_fila2]    
		dw_palletencahisto.Object.pahi_calle[ll_nueva2]  = dw_5.Object.paen_calle[ll_fila2]    
		dw_palletencahisto.Object.pahi_base[ll_nueva2] 	 = dw_5.Object.paen_base[ll_fila2]   
		dw_palletencahisto.Object.pahi_posici[ll_nueva2] = dw_5.Object.paen_posici[ll_fila2]    
		dw_palletencahisto.Object.pahi_estado[ll_nueva2] = dw_5.Object.paen_estado[ll_fila2]    
		dw_palletencahisto.Object.pahi_inspec[ll_nueva2] = dw_5.Object.paen_inspec[ll_fila2]    
		dw_palletencahisto.Object.pahi_concal[ll_nueva2] = dw_5.Object.paen_concal[ll_fila2]    
		dw_palletencahisto.Object.pahi_pexpor[ll_nueva2] = dw_5.Object.paen_pexpor[ll_fila2]
		dw_palletencahisto.Object.pahi_pmixto[ll_nueva2] = dw_5.Object.paen_pmixto[ll_fila2]
	NEXT
	
	FOR ll_fila3 = 1 TO dw_6.RowCount() 
	
		ll_nueva3 = dw_palletfrutahisto.InsertRow(0)

		dw_palletfrutahisto.Object.clie_codigo[ll_nueva3] = li_cliente 
		dw_palletfrutahisto.Object.paen_numero[ll_nueva3] = dw_6.Object.paen_numero[ll_fila3]    
		dw_palletfrutahisto.Object.espe_codigo[ll_nueva3] = dw_6.Object.espe_codigo[ll_fila3]  
		dw_palletfrutahisto.Object.vari_codigo[ll_nueva3] = dw_6.Object.vari_codigo[ll_fila3]    
		dw_palletfrutahisto.Object.emba_codigo[ll_nueva3] = dw_6.Object.emba_codigo[ll_fila3]    
		dw_palletfrutahisto.Object.prod_codigo[ll_nueva3] = dw_6.Object.prod_codigo[ll_fila3]    
		dw_palletfrutahisto.Object.cond_codigo[ll_nueva3] = dw_6.Object.cond_codigo[ll_fila3]    
		dw_palletfrutahisto.Object.etiq_codigo[ll_nueva3] = dw_6.Object.etiq_codigo[ll_fila3]     
		dw_palletfrutahisto.Object.plde_codigo[ll_nueva3] = li_planta  
		dw_palletfrutahisto.Object.pafh_calibr[ll_nueva3] = dw_6.Object.pafr_calibr[ll_fila3]    
		dw_palletfrutahisto.Object.pafh_proces[ll_nueva3] = ll_numero  
		dw_palletfrutahisto.Object.pafh_secuen[ll_nueva3] = dw_6.Object.pafr_secuen[ll_fila3]    
		dw_palletfrutahisto.Object.pafh_ccajas[ll_nueva3] = dw_6.Object.pafr_ccajas[ll_fila3]    
		dw_palletfrutahisto.Object.pafh_nrlote[ll_nueva3] = dw_6.Object.pafr_nrlote[ll_fila3]    
		dw_palletfrutahisto.Object.pafr_fecing[ll_nueva3] = dw_6.Object.pafr_fecing[ll_fila3]    
		dw_palletfrutahisto.Object.pafr_fecemb[ll_nueva3] = dw_6.Object.pafr_fecemb[ll_fila3]    
		dw_palletfrutahisto.Object.pafr_copack[ll_nueva3] = dw_6.Object.pafr_copack[ll_fila3]    
		dw_palletfrutahisto.Object.pafr_tipdoc[ll_nueva3] = 5 
		dw_palletfrutahisto.Object.pafr_huert1[ll_nueva3] = dw_6.Object.pafr_huert1[ll_fila3]  
		dw_palletfrutahisto.Object.pafr_cuart1[ll_nueva3] = dw_6.Object.pafr_cuart1[ll_fila3] 
		dw_palletfrutahisto.Object.pafr_varrot[ll_nueva3] = dw_6.Object.pafr_varrot[ll_fila3] 
	NEXT
NEXT
	
IF dw_alpalletencab.Rowcount() > 0 THEN
	lb_AutoCommit		=	sqlca.AutoCommit
	sqlca.AutoCommit	=	False
	
	IF dw_alpalletencab.Update(True, False) = 1 THEN
		IF dw_alpalletfruta.Update(True, False) = 1 THEN
			IF dw_palletencahisto.Update(True, False) = 1 THEN
				IF dw_palletfrutahisto.Update(True, False) = 1 THEN
					Commit;
					
					IF sqlca.SQLCode <> 0 THEN
				//		F_ErrorBaseDatos(sqlca, This.Title)
						
						RollBack;
						Return 
					ELSE
						lb_Retorno	=	True
						
						dw_alpalletencab.ResetUpdate()
						dw_alpalletfruta.ResetUpdate()
						dw_palletencahisto.ResetUpdate()
						dw_palletfrutahisto.ResetUpdate()
					END IF
					
				ELSE	
						F_ErrorBaseDatos(sqlca, This.Title)
			
					RollBack;
					Return 
				END IF	
			ELSE	
					F_ErrorBaseDatos(sqlca, This.Title)
			
					RollBack;
					Return 
			END IF		
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
			Return 
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
		
		Return 
	END IF
	
	Update dba.despafrigoen Set
	defe_proces = :ll_numero
	where defe_numero = :ll_despacho
	and	clie_codigo = :li_cliente
	and	plde_codigo = :li_planta
	Using Sqlca;
	
	sqlca.AutoCommit	=	lb_AutoCommit
	
	Return 
END IF
								  


end event

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.SetTabOrder("plde_codigo",10)
	dw_2.SetTabOrder("defe_numero",20)
	dw_2.Modify("defe_numero.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("plde_codigo.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.SetTabOrder("defe_fecdes",40)
	dw_2.Modify("defe_fecdes.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.SetTabOrder("defe_horade",50)
	dw_2.Modify("defe_horade.BackGround.Color = " + String(rgb(255,255,255)))
	
//	dw_2.SetTabOrder("sire_codigo",0)
//	dw_2.Modify("sire_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	
	dw_2.SetColumn("defe_numero")
	dw_2.SetFocus()
ELSE
	dw_2.SetTabOrder("defe_numero",0)
	dw_2.SetTabOrder("plde_codigo",0)
	dw_2.Modify("defe_numero.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("plde_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.SetTabOrder("defe_fecdes",0)
	dw_2.Modify("defe_fecdes.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.SetTabOrder("defe_horade",0)
	dw_2.Modify("defe_horade.BackGround.Color = " + String(RGB(166,180,210)))
END IF

//	dw_2.SetTabOrder("sire_codigo",0)
//	dw_2.Modify("sire_codigo.BackGround.Color = " + String(RGB(166,180,210)))
end subroutine

public subroutine habilitaingreso (string columna);Date	ld_fecha
Boolean	lb_estado = True

dw_2.AcceptText()

IF dw_2.RowCount() > 0 THEN
	//IsNull(dw_2.Object.defe_numero[1]) OR dw_2.Object.defe_numero[1] = 0 OR &
	IF IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &		
		IsNull(dw_2.Object.defe_fecdes[1]) OR dw_2.Object.defe_fecdes[1] = ld_fecha OR &
		IsNull(dw_2.Object.defe_cancaj[1]) OR dw_2.Object.defe_cancaj[1] = 0 OR &
		IsNull(dw_2.Object.defe_cantar[1]) OR dw_2.Object.defe_cantar[1] = 0 THEN
		
		lb_estado = False
	END IF
END IF

pb_grabar.Enabled		=	lb_estado
pb_ins_det.Enabled  	= 	lb_estado
pb_eli_det.Enabled  	= 	lb_estado
pb_imprimir.Enabled 	= 	lb_estado
end subroutine

public function boolean noexisteembarque (string as_embarque);String	ls_Nombre, ls_recibidor
Integer	li_Puerto, li_Cliente, li_Destino

li_Cliente	=	Integer(istr_mant.Argumento[3])

SELECT	em.embq_nomnav, em.puer_codigo, em.dest_codigo, re.cons_nombre
	INTO	:ls_Nombre, :li_Puerto, :li_Destino, :ls_recibidor
	FROM	dba.embarqueprod as em, dba.consignatario as re
	WHERE	em.clie_codigo	=	:li_Cliente
	AND	em.embq_codigo	=	:as_Embarque
   AND   re.cons_codigo	=	em.embq_clifac;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Embarqueprod")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Embarque no Existe. Ingrese Otra.", Exclamation!, Ok!)
	RETURN True
ELSE
	dw_2.SetItem(il_fila, "embq_nomnav", ls_Nombre)
	dw_2.SetItem(il_fila, "puer_codigo", li_Puerto)
	dw_2.SetItem(il_fila, "reci_nombre", ls_recibidor)
	
	istr_mant.Argumento[5]	=	as_Embarque
	istr_mant.Argumento[7]	=	String(li_Destino)
	
	RETURN False
END IF
end function

public subroutine buscaembarque ();Str_busqueda	lstr_busq

dw_2.Modify("buscaembarque.border = 0")
dw_2.Modify("buscaembarque.border = 5")

lstr_busq.argum[1]	=	istr_mant.argumento[3] // Cliente.

OpenWithParm(w_busc_embarques_consignatario, lstr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	dw_2.SetItem(il_fila, "embq_codigo", istr_busq.argum[1])
	dw_2.SetItem(il_fila, "embq_nomnav", istr_busq.argum[2])
	dw_2.SetItem(il_fila, "puer_codigo", Integer(istr_busq.argum[4]))
   	
	noexisteembarque(istr_busq.argum[1])
	
	istr_mant.argumento[7]	=	istr_busq.argum[5]
ELSE
	dw_2.SetColumn("embq_codigo")
	dw_2.SetFocus()
END IF

dw_2.Modify("buscaembarque.border = 0")
dw_2.Modify("buscaembarque.border = 6")

end subroutine

public function boolean conexionbase ();String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta
Integer	li_Planta

DISCONNECT USING sqlconec;

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

li_planta	=	dw_2.Object.defe_plades[1]

  SELECT pro.cone_nomodb,pro.cone_nomser,pro.cone_nombas,
         pro.cone_nodbms,pro.cone_nomusu,pro.cone_passwo  
    INTO :ls_nomodb,:ls_nomser,:ls_nombas,
	      :ls_nodbms,:ls_Usuario,:ls_Password
    FROM dba.plantadesp as pla,dba.prodconectividad as pro  
   WHERE pla.cone_codigo = pro.cone_codigo  and  
         pla.plde_codigo = :li_planta ;

sqlconec.ServerName	=	ls_nomser
sqlconec.DataBase	   =	ls_nombas
sqlconec.Dbms			= 	ls_nodbms
sqlconec.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'

CONNECT USING sqlconec;

IF sqlconec.SQLCode = 0 THEN
	ib_Conectado	=	True
ELSE
	ib_Conectado	=	False
END IF

RETURN ib_Conectado

end function

public function long buscanuevofolio (integer planta);/* Busca Folio para hacer una recepción de pallet a partir de
   un despacho de interpanta
	usando conextividad*/

Integer	li_planta
Long		ll_numero
Boolean	lb_nulo

li_planta	=	planta

SELECT Max(rfpe_numero)
  INTO :ll_numero
  FROM DBA.RECFRUPROCEE_TRANS
 WHERE plde_codigo = :li_planta
 USING sqlconec;

IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla RecFruProcee_Trans")
ELSEIF sqlconec.SQLCode = 0 THEN
	
		 lb_nulo = IsNull(ll_numero)	
	
	    IF lb_nulo THEN
			 ll_numero=li_planta*10000
		 ELSE
			 ll_numero++
		 END IF
ELSE
	ll_numero=li_planta*10000
END IF

RETURN ll_numero
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE
	IF ids_CorrelMovim.Update()	=	1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			IF dw_1.Update(True, False) = 1 THEN
				IF ids_palletfruta_fecha.Update()=1 THEN		
					Commit;
					
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
						
						RollBack;
					ELSE
						lb_Retorno	=	True
						
						dw_1.ResetUpdate()
						dw_2.ResetUpdate()
						ids_palletfruta_fecha.ResetUpdate()
						ids_CorrelMovim.ResetUpdate()
					END IF
				ELSE
					F_ErrorBaseDatos(sqlca, This.Title)
				
					RollBack;				
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean wf_actualiza_carga (boolean borrando);
Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlconec.AutoCommit
sqlconec.AutoCommit	=	False

		IF dw_7.Update(True, False) = 1 THEN
			IF dw_8.Update(True, False) = 1 THEN
				IF dw_3.Update(True, False) = 1 THEN
					IF dw_4.Update(True, False) = 1 THEN
						IF dw_11.Update(True, False) = 1 THEN
							IF dw_12.Update(True, False) = 1 THEN
								IF dw_15.Update(True, False)	=	1	THEN
									Commit;
										
									IF sqlconec.SQLCode <> 0 THEN
										F_ErrorBaseDatos(sqlconec, This.Title)
										RollBack;
									ELSE
										lb_Retorno	=	True
																
										dw_7.ResetUpdate()
										dw_8.ResetUpdate()
										dw_3.ResetUpdate()
										dw_4.ResetUpdate()
										dw_11.ResetUpdate()
										dw_12.ResetUpdate()
										dw_15.ResetUpdate()										
									END IF
								ELSE
									F_ErrorBaseDatos(sqlconec, This.Title)
									RollBack;
								END IF
							ELSE
								F_ErrorBaseDatos(sqlconec, This.Title)
								RollBack;
							END IF
						
						ELSE
							F_ErrorBaseDatos(sqlconec, This.Title)
							RollBack;
						END IF
					ELSE
						F_ErrorBaseDatos(sqlconec, This.Title)
						RollBack;
					END IF
				ELSE
					F_ErrorBaseDatos(sqlconec, This.Title)
					RollBack;
				END IF
			ELSE
				F_ErrorBaseDatos(sqlconec, This.Title)
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlconec, This.Title)
			RollBack;
		END IF

sqlconec.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine carga_inspeccion (long pallet);Integer	li_Cliente, li_Planta
Long   	ll_fila, ll_numero, ll_fildet, ll_cuenta, ll_Existe
String 	ls_numero

li_Cliente	= dw_2.Object.clie_codigo[1]
li_Planta	= dw_2.Object.defe_plades[1]


 IF  dw_9.Retrieve(ii_tipoin,il_numins,dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],ii_secuen) > 0 THEN

	ll_numero = dw_2.Object.plde_codigo[1] * 1000000 + il_numins
	
	SELECT Count()
		INTO :ll_Existe
		FROM dba.dba.inspecpalenc_trans
		WHERE inpe_tipoin = :ii_tipoin  AND
				inpe_numero = :ll_numero  AND
				clie_codigo = :li_Cliente AND
				plde_codigo = :li_Planta  AND
				inpe_secuen = :ii_secuen
		USING sqlconec;
				
	IF ll_Existe > 0 THEN 		
	ELSE			
		ll_fila	= dw_11.Find("inpe_tipoin = " + String(ii_tipoin) + &
									 " AND inpe_numero = " + String(ll_numero) + &
							  		 " AND clie_codigo = " + String(dw_2.Object.clie_codigo[1]) + &
							       " AND plde_codigo = " + String(dw_2.Object.defe_plades[1]) + &
							  		 " AND inpe_secuen = " + String(ii_secuen) , 1 , dw_11.RowCount())
							  
		IF ll_fila = 0 THEN
			dw_9.RowsCopy(1,dw_9.RowCount(), Primary!, dw_11, 1, Primary!)
			dw_11.Object.inpe_numero[1] = ll_numero
			dw_11.Object.plde_codigo[1] = dw_2.Object.defe_plades[1]
		END IF
	END IF
END IF

ll_cuenta=0

IF dw_10.Retrieve(ii_tipoin,il_numins,dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],ii_secuen,pallet) > 0 THEN

		ll_fildet= dw_12.Find("inpe_tipoin = " + String(ii_tipoin) + &
							  		 " AND inpe_numero = " + String(ll_numero) + &
							   	 " AND clie_codigo = " + String(dw_2.Object.clie_codigo[1]) + &
							  		 " AND plde_codigo = " + String(dw_2.Object.defe_plades[1]) + &
							  		 " AND inpe_secuen = " + String(ii_secuen) + &
							  		 " AND paen_numero = " + String(pallet), 1 , dw_12.RowCount())
							  
		IF ll_fildet = 0 THEN
			dw_10 .RowsCopy(1,dw_10.RowCount(), Primary!, dw_12, 1, Primary!)
			dw_12.Object.inpe_numero[1] = ll_numero
	      dw_12.Object.plde_codigo[1] = dw_2.Object.defe_plades[1]
		END IF
	
END IF
					  		
						 
							 
						 
end subroutine

public function boolean busca_numeroinspeccion (integer cliente, integer planta, long pallet);Date ld_fecha
Long ll_num_inspec

il_numins = 0 ; ii_tipoin = 0 ; ii_secuen = 0 ;

   SELECT Max(inpd_fechai) 
	  INTO :ld_fecha
	  FROM dba.inspecpaldet 	
	 WHERE clie_codigo = :Cliente AND  
			 plde_codigo = :Planta  AND  
			 paen_numero = :Pallet; 
				 
	SELECT inpe_numero, inpe_tipoin, inpe_secuen 
	  INTO :il_numins, :ii_tipoin, :ii_secuen  
	  FROM dba.inspecpaldet 	AS i 
	 WHERE clie_codigo = :Cliente AND  
			 plde_codigo = :Planta  AND  
			 paen_numero = :Pallet  AND
			 inpd_fechai = :ld_fecha; 
			 
IF il_numins > 0 THEN
	RETURN TRUE
ELSE
	RETURN FALSE
END IF


end function

public subroutine existe_cargaregistro ();Long ll_Filrec, ll_Filpal, ll_Numero, ll_Existe, ll_Pallet, ll_Filinp, ll_Numinp, ll_Secuen, ll_Totfil
Integer li_Planta, li_Cliente, li_Tipo

IF dw_2.RowCount() > 0 THEN
	
	li_Planta 	= dw_2.Object.defe_plades[1]
	ll_Numero 	= dw_2.Object.defe_numero[1]
	li_Cliente 	= dw_2.Object.clie_codigo[1]
	
	SELECT rfpe_numero  
	  INTO :ll_Existe  
	  FROM dba.recfruprocee_trans 
	 WHERE plde_codigo = :li_Planta  AND  
			 clie_codigo = :li_Cliente AND  
			 rfpe_numero = :ll_Numero   
	USING sqlconec ;
	 
	IF ll_Existe > 0 THEN		
			DELETE FROM dba.recfruproced_trans 
   		 WHERE plde_codigo = :li_Planta AND  
                rfpe_numero = :ll_Existe  AND  
                clie_codigo = :li_Cliente 
          USING sqlconec ;	
							
			DELETE FROM dba.recfruprocee_trans  
			 WHERE plde_codigo = :li_Planta AND  
			       rfpe_numero = :ll_Existe  AND  
				    clie_codigo = :li_Cliente 
		   USING sqlconec ;
	END IF
 
  END IF
COMMIT ;

end subroutine

public subroutine generaarchivoplano ();Integer	li_Tabla, li_trans
String	ls_Archivo,ls_Registro
Boolean	lb_Anulacion=True, lb_cambia = False
Long		ll_Fila, ll_FilPal, ll_Filadet, ll_Filinp, ll_Fildet, ll_filins

dw_11.SettransObject(sqlca)
dw_12.SettransObject(sqlca)
dw_11.Reset()
dw_12.Reset()
	
dw_13.Reset()

Open(w_mensaje)
w_mensaje.st_1.text = " Generando Archivo Plano"

IF	dw_2.RowCount() > 0 THEN
   ls_Archivo	=	String(dw_2.Object.defe_plades[1], '0000') + &
							String(dw_2.Object.defe_guides[1],'00000000') + &
							".Txt"
							
	li_Tabla		= 1	
	ls_Registro	=	String(li_Tabla)
	ls_Registro	+=	String(dw_2.Object.defe_plades[1], '0000')
	ls_Registro	+=	String(dw_2.Object.defe_numero[1], '00000000')
	ls_Registro	+=	String(dw_2.Object.clie_codigo[1], '000')
	ls_Registro += String(Today(), 'ddmmyyyy')
	ls_Registro	+=	String(2,'00')
	ls_Registro	+=	String(dw_2.Object.plde_codigo[1], '0000')
	ls_Registro	+=	String(dw_2.Object.defe_guides[1], '00000000')
	IF	IsNull(dw_2.Object.tran_codigo[1]) THEN
	 	ls_Registro += FILL('0',4)
	ELSE
		ls_Registro	+=	String(dw_2.Object.tran_codigo[1], '0000')
	END IF
	 
	IF IsNull(dw_2.Object.tica_codigo[1]) THEN
		ls_Registro += FILL('0',2)
	ELSE
		ls_Registro	+=	String(dw_2.Object.tica_codigo[1], '00')
	END IF
	 
	IF IsNull(dw_2.Object.defe_patent[1]) THEN
		ls_Registro += FILL(' ', 20)
	ELSE
		ls_Registro	+=	String(dw_2.Object.defe_patent[1], Fill('@', 20))
	END IF
	 
	IF IsNull(dw_2.Object.defe_chofer[1]) THEN
		ls_Registro += FILL(' ', 50)
	ELSE
		ls_Registro	+=	String(dw_2.Object.defe_chofer[1], Fill('@', 50))
	END IF 
	IF IsNull(dw_2.Object.defe_fecact[1]) THEN
		ls_Registro 	+= Fill('0',8)
	ELSE
	 	ls_Registro 	+= String(dw_2.Object.defe_fecact[1], 'ddmmyyyy')
	END IF
	IF IsNull(dw_2.Object.defe_horact[1]) THEN
		ls_Registro 	+= Fill('0',6)
	ELSE
	   ls_Registro 	+= String(dw_2.Object.defe_horact[1], 'hhmmss')
	END IF
	
		dw_13.InsertRow(0)
	dw_13.Object.registro[1] = ls_Registro
	
	FOR ll_Fila = 1 TO dw_1.RowCount()
		ls_Registro	=	String(2) /*Recfruproced*/
		ls_Registro	+=	String(dw_2.Object.defe_plades[1], '0000')
		ls_Registro	+=	String(dw_1.Object.defe_numero[ll_Fila], '00000000')
		ls_Registro	+=	String(dw_1.Object.clie_codigo[ll_Fila], '000')
		ls_Registro	+=	String(dw_1.Object.paen_numero[ll_Fila], '00000000')
		  
		ll_filadet	=	dw_13.InsertRow(0)
		dw_13.Object.registro[ll_filadet]	=	ls_Registro
		  
		dw_5.GetChild("plde_codigo", dw_planta)
	   dw_planta.SetTransObject(sqlca)
	   dw_planta.Retrieve(1)
		  
		IF dw_5.Retrieve(dw_2.Object.clie_codigo[1],dw_1.Object.paen_numero[ll_Fila],dw_2.Object.plde_codigo[1]) > 0 THEN
			
	 	  	ls_Registro	=	String(3) /*PalletEncab*/	
		  	ls_Registro	+=	String(dw_5.Object.clie_codigo[1], '000')
		  	ls_Registro	+=	String(dw_5.Object.paen_numero[1], '00000000')
		  	ls_Registro	+=	String(dw_2.Object.defe_plades[1], '0000')
		  	IF IsNull(dw_5.Object.paen_tipopa[1]) THEN
		  		ls_Registro += FILL('0',1)
	     	ELSE
	        	ls_Registro	+=	String(dw_5.Object.paen_tipopa[1], '0')
	     	END IF
		  	IF IsNull(dw_5.Object.tpem_codigo[1]) THEN
		   	ls_Registro += FILL(' ',5)
		  	ELSE
				ls_Registro	+=	String(dw_5.Object.tpem_codigo[1],  Fill('@', 5))
		  	END IF
		  	IF IsNull(dw_5.Object.espe_codigo[1]) THEN
				ls_Registro += FILL('0',2)
		  	ELSE
		   	ls_Registro	+=	String(dw_5.Object.espe_codigo[1], '00')
		  	END IF
		  	IF IsNull(dw_5.Object.vari_codigo[1]) THEN
				ls_Registro += FILL('0',4)
		  	ELSE
		   	ls_Registro	+=	String(dw_5.Object.vari_codigo[1], '0000')
		  	END IF
		  	IF IsNull(dw_5.Object.tiem_codigo[1]) THEN
				ls_Registro += FILL('0',2)
		  	ELSE
		   	ls_Registro	+=	String(dw_5.Object.tiem_codigo[1], '00')
		  	END IF
		  	IF IsNull(dw_5.Object.cate_codigo[1]) THEN
			  	ls_Registro += FILL('0',3)
		  	ELSE
		   	ls_Registro	+=	String(dw_5.Object.cate_codigo[1], '000')
		  	END IF
		  	IF IsNull(dw_5.Object.etiq_codigo[1]) THEN
				ls_Registro += FILL('0',4)
		  	ELSE
		   	ls_Registro	+=	String(dw_5.Object.etiq_codigo[1], '0000')
		  	END IF
		  	IF IsNull(dw_5.Object.stat_codigo[1]) THEN
				ls_Registro += FILL('0',3)
		  	ELSE
		   	ls_Registro	+=	String(dw_5.Object.stat_codigo[1], '000')
		  	END IF
		  	IF IsNull(dw_5.Object.trat_codigo[1]) THEN
				ls_Registro += FILL('0',2)
			ELSE
		 		ls_Registro	+=	String(dw_5.Object.trat_codigo[1], '00')
		  	END IF
			IF IsNull(dw_5.Object.frio_codigo[1]) THEN
				ls_Registro += FILL(' ',1)
			ELSE
		  		ls_Registro	+=	String(dw_5.Object.frio_codigo[1],  Fill('@', 1))
		  	END IF
			IF IsNull(dw_5.Object.cond_codigo[1]) THEN
				ls_Registro += FILL('0',1)
			ELSE
				ls_Registro	+=	String(dw_5.Object.cond_codigo[1], '0')
			END IF
		   IF IsNull(dw_5.Object.dest_codigo[1]) THEN
				ls_Registro += FILL('0',3)
			ELSE
			  	ls_Registro	+=	String(dw_5.Object.dest_codigo[1], '000')
			END IF
			IF IsNull(dw_5.Object.emba_codigo[1]) THEN
				ls_Registro += FILL(' ',10)
			ELSE
				ls_Registro	+=	String(dw_5.Object.emba_codigo[1], Fill('@', 10))
			END IF
			IF IsNull(dw_5.Object.paen_fecemb[1]) THEN
				ls_Registro += FILL('0',8)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_fecemb[1], 'ddmmyyyy')
			END IF  
			IF IsNull(dw_5.Object.paen_cosecha[1]) THEN
				ls_Registro += FILL('0',8)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_cosecha[1], 'ddmmyyyy')
		   END IF
			IF IsNull(dw_5.Object.paen_altura[1]) THEN
				ls_Registro += FILL('0',4)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_altura[1], '0000')
		   END IF 
			IF IsNull(dw_5.Object.paen_ccajas[1]) THEN
				ls_Registro += FILL('0',7)
			ELSE
			  	ls_Registro	+=	String(dw_5.Object.paen_ccajas[1], '0000000')
			END IF
			IF IsNull(dw_5.Object.tmvp_codigo[1]) THEN
				ls_Registro += FILL('0',2)
			ELSE
			  ls_Registro	+=	String(dw_5.Object.tmvp_codigo[1], '00')
		   END IF
			IF	IsNull(dw_5.Object.paen_fecini[1]) THEN
				ls_Registro += FILL('00000000',8)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_fecini[1],  'ddmmyyyy')
		   END IF
			IF IsNull(dw_5.Object.paen_horain[1]) THEN
				ls_Registro += FILL('0',6)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_horain[1], 'hhmmss')
		   END IF
			ls_Registro += FILL('0',4) 	//Camara
			ls_Registro += FILL('0',2) 	//Calle
			ls_Registro += FILL('0',2) 	//Base
			ls_Registro += FILL('0',1) 	//Posición
			ls_Registro	+=	String(1, '0') //Estado
		   
			IF IsNull(dw_5.Object.paen_inspec[1]) THEN 
				ls_Registro += FILL('0',1)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_inspec[1], '0')
		   END IF
			IF IsNull(dw_5.Object.paen_concal[1]) THEN  
				ls_Registro += FILL('0',1)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_concal[1], '0')
		   END IF
			IF IsNull(dw_5.Object.paen_pexpor[1]) THEN  
				ls_Registro += FILL('0',1)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_pexpor[1], '0')
		   END IF
			IF IsNull(dw_5.Object.paen_pmixto[1]) THEN 
				ls_Registro += FILL('0',1)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_pmixto[1], '0')
		   END IF
			IF IsNull(dw_5.Object.paen_varrot[1]) THEN
				ls_Registro += FILL('0',4)
			ELSE			  
				ls_Registro	+=	String(dw_5.Object.paen_varrot[1], '0000')
		   END IF
			IF IsNull(dw_5.Object.paen_nrasda[1]) THEN
				ls_Registro += FILL(' ',16)
			ELSE
				ls_Registro	+=	String(dw_5.Object.paen_nrasda[1] ,Fill('@', 16))
		   END IF
			IF NOT IsNull(dw_5.Object.copa_codigo[1]) THEN
				ls_Registro	+=	String(dw_5.Object.copa_codigo[1], '000')
		   END IF
			  
			ll_filadet	=	dw_13.InsertRow(0)
		   dw_13.Object.registro[ll_filadet]	=	ls_Registro
			  
			IF	dw_6.Retrieve(dw_2.Object.clie_codigo[1],dw_1.Object.paen_numero[ll_fila],dw_2.Object.plde_codigo[1]) > 0 THEN
			  				
	 			FOR ll_FilPal = 1 TO dw_6.RowCount()
			 		ls_Registro	=	String(4) /*PalletFruta*/	
					ls_Registro	+=	String(dw_6.Object.clie_codigo[ll_FilPal], '000')
					ls_Registro	+=	String(dw_6.Object.paen_numero[ll_FilPal], '00000000')
					ls_Registro	+=	String(dw_6.Object.espe_codigo[ll_FilPal], '00')
					ls_Registro	+=	String(dw_6.Object.vari_codigo[ll_FilPal], '0000')
					ls_Registro	+=	String(dw_6.Object.emba_codigo[ll_FilPal] ,Fill('@', 10))
					ls_Registro	+=	String(dw_6.Object.prod_codigo[ll_FilPal], '00000')
					ls_Registro	+=	String(dw_6.Object.cond_codigo[ll_FilPal], '0')
					ls_Registro	+=	String(dw_6.Object.etiq_codigo[ll_FilPal], '0000')
					ls_Registro	+=	String(dw_2.Object.defe_plades[1], '0000')
					ls_Registro	+=	String(iuo_calibre.fomato_calibre(dw_6.Object.pafr_calibr[ll_FilPal],sqlca))
					ls_Registro	+=	String(dw_6.Object.pafr_secuen[ll_FilPal], '00')
					IF IsNull(dw_6.Object.pafr_ccajas[ll_FilPal]) THEN
						ls_Registro += FILL('0',7)
					ELSE
						ls_Registro	+=	String(dw_6.Object.pafr_ccajas[ll_FilPal], '0000000')
					END IF
					IF IsNull(dw_6.Object.pafr_nrlote[ll_FilPal]) THEN
						ls_Registro += FILL('0',4)
					ELSE
						ls_Registro	+=	String(dw_6.Object.pafr_nrlote[ll_FilPal], '0000')
					END IF
					IF IsNull(dw_6.Object.pafr_copack[ll_FilPal]) THEN
						ls_Registro += FILL('0',4)
					ELSEIF dw_6.Object.pafr_copack[ll_FilPal] = -1 THEN
						ls_Registro	+=	String(dw_6.Object.pafr_copack[ll_FilPal], '000')
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_copack[ll_FilPal], '0000')
					END IF
					IF IsNull(dw_6.Object.pafr_varrot[ll_FilPal]) THEN
						ls_Registro += FILL('0',4)
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_varrot[ll_FilPal], '0000')
					END IF
					IF IsNull(dw_6.Object.pafr_prdrot[ll_FilPal]) THEN
						ls_Registro += FILL('0',5)
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_prdrot[ll_FilPal], '00000')
					END IF
					IF IsNull(dw_6.Object.pafr_calrot[ll_FilPal]) THEN
						ls_Registro += FILL(' ',3)
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_calrot[ll_FilPal],  Fill('@', 3))
					END IF
					IF IsNull(dw_6.Object.pafr_huert1[ll_FilPal]) THEN
						ls_Registro += FILL('0',5)
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_huert1[ll_FilPal], '00000')
					END IF
					IF IsNull(dw_6.Object.pafr_cuart1[ll_FilPal]) THEN
						ls_Registro += FILL('0',5)
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_cuart1[ll_FilPal], '00000')
					END IF
					IF IsNull(dw_6.Object.pafr_fecemb[ll_FilPal]) THEN
						ls_Registro += FILL('0',8)
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_fecemb[ll_FilPal], 'ddmmyyyy')
					END IF
					IF IsNull(dw_6.Object.pafr_fecing[ll_FilPal]) THEN
						ls_Registro += FILL('0',8)
					ELSE
					   ls_Registro	+=	String(dw_6.Object.pafr_fecing[ll_FilPal], 'ddmmyyyy')
					END IF					
										
					ll_filadet	=	dw_13.InsertRow(0)
		         dw_13.Object.registro[ll_filadet]	=	ls_Registro
				NEXT
			END IF
			
			IF dw_14.Retrieve(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_1.Object.paen_numero[ll_fila]) > 0 THEN	
				FOR ll_FilPal = 1 TO dw_14.RowCount()
			 		ls_Registro	=	String(7) /*Spro_Cajasprod*/	
					ls_Registro	+=	String(dw_14.Object.clie_codigo[ll_FilPal], '000')	
					ls_Registro	+=	String(dw_2.Object.defe_plades[1], '0000')
					ls_Registro	+=	String(dw_14.Object.capr_numero[ll_FilPal], '00000000')
					ls_Registro	+=	String(dw_14.Object.espe_codigo[ll_FilPal], '00')
					ls_Registro	+=	String(dw_14.Object.vari_codigo[ll_FilPal], '0000')
					ls_Registro	+=	String(dw_14.Object.prod_codigo[ll_FilPal], '00000')	
					IF IsNull(dw_14.Object.prod_predio[ll_FilPal]) THEN
						ls_Registro	+=	'000'							
					ELSE
						ls_Registro	+=	String(dw_14.Object.prod_predio[ll_FilPal], '000')	
					END IF
					
					IF IsNull(dw_14.Object.prod_huerto[ll_FilPal]) THEN
						ls_Registro	+=	'000'					
					ELSE
	 					ls_Registro	+=	String(dw_14.Object.prod_huerto[ll_FilPal], '000')	
					END IF
					
					IF IsNull(dw_14.Object.prod_cuarte[ll_FilPal]) THEN
						ls_Registro	+=	'000'					
					ELSE
						ls_Registro	+=	String(dw_14.Object.prod_cuarte[ll_FilPal], '000')						 
					END IF
					
					ls_Registro	+=	String(dw_14.Object.emba_codigo[ll_FilPal] ,Fill('@', 10))	
					IF IsNull(dw_14.Object.etiq_codigo[ll_FilPal]) THEN
						ls_Registro	+=	'000'					
					ELSE
						ls_Registro	+=	String(dw_14.Object.etiq_codigo[ll_FilPal], '000')	
					END IF
					IF IsNull(dw_14.Object.capr_fecemb[ll_FilPal]) THEN
						ls_Registro += FILL('0',8)
					ELSE
					   ls_Registro	+=	String(dw_14.Object.capr_fecemb[ll_FilPal], 'ddmmyyyy')
					END IF
					ls_Registro	+=	String(iuo_calibre.fomato_calibre(dw_14.Object.capr_calibr[ll_FilPal],sqlca)) 
					IF IsNull(dw_14.Object.capr_embala[ll_FilPal]) THEN
						ls_Registro += FILL('0',8)						
					ELSE
						ls_Registro	+=	String(dw_14.Object.capr_embala[ll_FilPal], '00000000')
					END IF
					IF IsNull(dw_14.Object.capr_selecc[ll_FilPal]) THEN
						ls_Registro += FILL('0',8)						
					ELSE
						ls_Registro	+=	String(dw_14.Object.capr_selecc[ll_FilPal], '00000000')
					END IF
					IF IsNull(dw_14.Object.capr_pesado[ll_FilPal]) THEN
						ls_Registro += FILL('0',8)						
					ELSE
						ls_Registro	+=	String(dw_14.Object.capr_pesado[ll_FilPal], '00000000')
					END IF
					IF IsNull(dw_14.Object.capr_cean14[ll_FilPal]) THEN
						ls_Registro += FILL(' ',14)						
					ELSE
						ls_Registro	+=	String(dw_14.Object.capr_cean14[ll_FilPal] ,Fill('@', 14))	
					END IF
					ls_Registro	+=	String(dw_14.Object.capr_numpal[ll_FilPal], '00000000')
					ls_Registro	+=	String(dw_14.Object.capr_regcap[ll_FilPal] ,Fill('@', 100))	
					ls_Registro	+=	String(dw_14.Object.capr_estado[ll_FilPal], '0')			
					IF IsNull(dw_14.Object.capr_varrot[ll_FilPal]) THEN
						ls_Registro += FILL('0',4)									
					ELSE
						ls_Registro	+=	String(dw_14.Object.capr_varrot[ll_FilPal], '0000')	
					END IF
					IF IsNull(dw_14.Object.capr_numgia[ll_FilPal]) THEN
						ls_Registro += FILL('0',8)									
					ELSE
						ls_Registro	+=	String(dw_14.Object.capr_numgia[ll_FilPal], '00000000')	
					END IF
					ls_Registro	+=	String(dw_14.Object.cate_codigo[ll_FilPal], '000')					
					IF IsNull(dw_14.Object.capr_cespak[ll_FilPal]) THEN
						ls_Registro += FILL('0',4)															
					ELSE
						ls_Registro	+=	String(dw_14.Object.capr_cespak[ll_FilPal], '0000')	
					END IF
					ls_Registro	+=	String(dw_14.Object.capr_docrel[ll_FilPal], '00000000')	
					IF IsNull(dw_14.Object.capr_hordig[ll_FilPal]) THEN
						ls_Registro += FILL('0',6)
					ELSE
						ls_Registro	+=	String(dw_14.Object.capr_hordig[ll_FilPal], 'hhmmss')
					END IF
					
					IF IsNull(dw_14.Object.capr_fecdig[ll_FilPal]) THEN
						ls_Registro += FILL('0',8)
					ELSE
					   ls_Registro	+=	String(dw_14.Object.capr_fecdig[ll_FilPal], 'ddmmyyyy')
					END IF
					
					IF IsNull(dw_14.Object.capr_nrlote[ll_FilPal]) THEN
						ls_Registro += FILL('0',4)
					ELSE
						ls_Registro	+=	String(dw_14.Object.capr_nrlote[ll_FilPal], '0000')
					END IF				
										 
					ll_filadet	=	dw_13.InsertRow(0)
		         dw_13.Object.registro[ll_filadet]	=	ls_Registro
				NEXT
			END IF
			
   	END IF
		
		IF dw_1.Object.paen_inspec[ll_fila] = 1 THEN
			IF Busca_Numeroinspeccion(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_1.Object.paen_numero[ll_fila]) THEN
				carga_inspeccion(dw_1.Object.paen_numero[ll_fila])
			END IF
		END IF
	NEXT	
	
	IF dw_11.RowCount() > 0 THEN
		FOR ll_Filinp = 1 TO dw_11.RowCount()
			ls_Registro	=	String(5) /*Inspecpalenc*/	
			ls_Registro	+=	String(dw_11.Object.inpe_tipoin[ll_Filinp], '0')
			ls_Registro	+=	String(dw_11.Object.inpe_numero[ll_Filinp], '00000000')
			ls_Registro	+=	String(dw_11.Object.clie_codigo[ll_Filinp], '000')
			ls_Registro	+=	String(dw_11.Object.plde_codigo[ll_Filinp], '0000')
			ls_Registro	+=	String(dw_11.Object.inpe_secuen[ll_Filinp], '00')
			ls_Registro	+=	String(dw_11.Object.dest_codigo[ll_Filinp], '000')
			ls_Registro	+=	String(dw_11.Object.inpe_fechai[ll_Filinp], 'ddmmyyyy')
			
			IF IsNull(dw_11.Object.espe_codigo[ll_Filinp]) THEN
				ls_Registro += FILL('0',2)
			ELSE
				ls_Registro	+=	String(dw_11.Object.espe_codigo[ll_Filinp], '00')
			END IF
			IF IsNull(dw_11.Object.vari_codigo[ll_Filinp]) THEN
			   ls_Registro += FILL('0',4) 
			ELSE
				ls_Registro	+=	String(dw_11.Object.vari_codigo[ll_Filinp], '0000')
			END IF
			IF IsNull(dw_11.Object.emba_codigo[ll_Filinp]) THEN
			   ls_Registro += FILL(' ',10)
			ELSE
				ls_Registro	+=	String(dw_11.Object.emba_codigo[ll_Filinp], Fill('@', 10))
			END IF
			IF IsNull(dw_11.Object.tpem_codigo[ll_Filinp]) THEN
				ls_Registro += FILL(' ',5)
			ELSE
				ls_Registro	+=	String(dw_11.Object.tpem_codigo[ll_Filinp], Fill('@', 5))
			END IF
			IF IsNull(dw_11.Object.inpe_todpal[ll_Filinp]) THEN
				ls_Registro += FILL('0',1) 
			ELSE
				ls_Registro	+=	String(dw_11.Object.inpe_todpal[ll_Filinp], '0')
			END IF
			IF IsNull(dw_11.Object.inpe_calibr[ll_Filinp]) THEN
				ls_Registro += FILL(' ',3)
			ELSE
				ls_Registro	+=	String(iuo_calibre.fomato_calibre(dw_11.Object.inpe_calibr[ll_Filinp],sqlca))  
			END IF
			
			ll_filadet	=	dw_13.InsertRow(0)
		   dw_13.Object.registro[ll_filadet]	=	ls_Registro
		NEXT
	END IF
	IF dw_12.RowCount() > 0 THEN
		FOR ll_Fildet = 1 TO dw_12.RowCount()
			ls_Registro	=	String(6) /*Inspecpaldet*/	
			ls_Registro	+=	String(dw_12.Object.inpe_tipoin[ll_Fildet], '0')
			ls_Registro	+=	String(dw_12.Object.inpe_numero[ll_Fildet], '00000000')
			ls_Registro	+=	String(dw_12.Object.clie_codigo[ll_Fildet], '000')
			ls_Registro	+=	String(dw_12.Object.plde_codigo[ll_Fildet], '0000')
			ls_Registro	+=	String(dw_12.Object.inpe_secuen[ll_Fildet], '00')
			ls_Registro	+=	String(dw_12.Object.paen_numero[ll_Fildet], '00000000')
			IF IsNull(dw_12.Object.dest_codigo[ll_Fildet]) THEN
				ls_Registro += FILL('0',3) 
			ELSE
				ls_Registro	+=	String(dw_12.Object.dest_codigo[ll_Fildet], '000')
			END IF
			IF IsNull(dw_12.Object.inpd_fechai[ll_Fildet]) THEN
				ls_Registro += FILL('0',8) 
			ELSE
				ls_Registro	+=	String(dw_12.Object.inpd_fechai[ll_Fildet], 'ddmmyyyy')
			END IF
			IF IsNull(dw_12.Object.inpd_nroanu[ll_Fildet]) THEN
				ls_Registro += FILL('0',8) 
			ELSE
				ls_Registro	+=	String(dw_12.Object.inpd_nroanu[ll_Fildet], '00000000')
			END IF
			IF IsNull(dw_12.Object.inpd_fechaa[ll_Fildet]) THEN
				ls_Registro += FILL('0',8) 
			ELSE
				ls_Registro	+=	String(dw_12.Object.inpd_fechaa[ll_Fildet], 'ddmmyyyy')
			END IF
			IF IsNull(dw_12.Object.inpd_frecha[ll_Fildet]) THEN
				ls_Registro += FILL('0',8) 
			ELSE
				ls_Registro	+=	String(dw_12.Object.inpd_frecha[ll_Fildet], 'ddmmyyyy')
			END IF
	
	      ll_filadet	=	dw_13.InsertRow(0)
		   dw_13.Object.registro[ll_filadet]	=	ls_Registro
		NEXT	
	END IF
END IF

Close(w_mensaje)

IF dw_13.SaveAs("C:\GeneradosInterplanta\" + ls_Archivo, Text!, False) = -1 THEN
	MessageBox("Atención","No se pudo generar el archivo " + ls_Archivo)
ELSE
	MessageBox("Atención","Archivo Generado en directorio GeneradosInterplanta " + ls_Archivo)
END IF
end subroutine

public function long busnuevofoliodespa (integer ai_cliente, integer ai_planta);/* Busca Folio para hacer un  Despacho de pallet */

Integer	li_Planta
Long		ll_Numero, ll_Inicia, ll_Termin, ll_Actual, ll_Quedan, ll_despacho
Boolean	lb_Nulo

li_Planta	=	ai_planta
ll_Numero	=	0

select max(defe_numero)
into :ll_despacho
from dba.despafrigoen
where plde_codigo = :li_planta;
		
ids_CorrelMovim.Retrieve(li_Planta,2)
IF ids_CorrelMovim.RowCount() > 0 THEN
	ll_Inicia	=	ids_CorrelMovim.Object.como_inicia[1]
	ll_Termin	=	ids_CorrelMovim.Object.como_termin[1]
	ll_Actual	=	ids_CorrelMovim.Object.como_actual[1]
	
	IF Isnull(ll_Inicia) THEN ll_Inicia	=	0
	IF Isnull(ll_Termin) THEN ll_Termin	=	0
	IF Isnull(ll_Actual) THEN ll_Actual	=	0
		
	IF Isnull(ll_despacho) OR String(ll_despacho) = '' OR ll_despacho < ll_Inicia THEN
		ll_Actual = ll_Inicia
	ELSE
		ll_Actual=	ll_despacho
	END IF	

	IF ll_Inicia >= 0 AND ll_Termin > 0 THEN
		IF ll_Actual	=	0	THEN
			ll_Actual	=	ll_Inicia + 1
		ELSE
			ll_Actual++		
		END IF
		
		IF ll_Actual >= ll_Inicia AND	ll_Actual <= ll_Termin	THEN
		
			IF ll_Actual > ll_Termin THEN
				Messagebox("Atención","No Existen Números de Folios Disponibles de Movimiento de Despacho",exclamation!) 			
				ll_Numero	=	0
				RETURN ll_Numero
			ELSEIF ll_Actual = ll_Termin THEN
				Messagebox("Atención","Ultimo Número de Folio Disponible de Movimiento de Despacho",exclamation!) 			
			END IF
			
			ll_Numero	=	ll_Actual
				
			ll_Quedan	=	(ll_Termin - ll_Actual)
			IF ll_Quedan <= 3 THEN
				Messagebox("Atención","Existen "+String(ll_Quedan)+" Número de Folio Disponible de Movimiento de Despacho",exclamation!) 			
			END IF
			
			ids_CorrelMovim.Object.como_actual[1]	=	ll_Actual
		ELSE
			Messagebox("Atención","Número de Folio Generado no se encuentra Disponible dentro del rango de Movimiento de Despacho",exclamation!) 
			ll_Numero	=	0
			RETURN ll_Numero
		END IF		
		
	ELSE
		Messagebox("Atención","Número de Folio Generado no se encuentra Disponible dentro del rango de Movimiento de Despacho",exclamation!) 
		ll_Numero	=	0
		RETURN ll_Numero	
	END IF
	
ELSE
	Messagebox("Atención","No Existe Ningún Número de Movimiento de Despacho",exclamation!) 
	ll_Numero	=	0
	RETURN ll_Numero	
END IF

RETURN ll_numero
end function

public function boolean existefolio (string as_columna, string as_valor);Integer	li_planta, li_existe, li_cliente
Long		ll_nfolio

li_cliente	=	dw_2.Object.clie_codigo[1]
li_planta	=	dw_2.Object.plde_codigo[1]
ll_nfolio 	=	dw_2.Object.defe_numero[1]

CHOOSE CASE as_columna
	CASE "plde_codigo"
		li_planta	=	Integer(as_valor)
		
	CASE "defe_numero"
		ll_nfolio 	=	Long(as_valor)
		
END CHOOSE

SELECT	Count(*)
	INTO	:li_existe
	FROM	dba.DESPAFRIGOEN
	WHERE	plde_codigo	=	:li_planta
	AND	defe_numero	=	:ll_nfolio 
	And   clie_codigo =  :li_cliente ;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Despafrigoen")
	RETURN False
ELSEIF li_existe > 0 THEN
	istr_mant.argumento[1]	= String(li_planta)
	istr_mant.argumento[2]	= String(ll_nfolio)
	
	This.TriggerEvent("ue_recuperadatos")

	istr_mant.argumento[3]	= 	String(dw_2.Object.clie_codigo[1])
	istr_mant.argumento[4]	= 	String(dw_2.Object.defe_cantar[1])
	istr_mant.argumento[8]	=	String(dw_2.Object.defe_cancaj[1])
	ib_existe_folioD			=	True
   RETURN False
ELSE
	IF IsNull(ll_nfolio) THEN
		istr_mant.argumento[1]	= String(li_planta)
		istr_mant.argumento[2]	= String(ll_nfolio)
		//istr_mant.argumento[3]	= String(li_cliente)
		ib_existe_folioD			=	False
		RETURN False
	ELSE
	    MessageBox("Atención","Número de Documento No ha sido generado. Ingrese Otro.")
		ib_existe_folioD			=	False
	   RETURN True
	END IF	
END IF





end function

public function boolean buscaregistros ();Integer  li_existe, li_planta, li_cliente
Long		ll_nfolio

//li_planta = dw_2.Object.plde_codigo[1]
li_planta = dw_2.Object.defe_plades[1]
li_cliente = dw_2.Object.clie_codigo[1]
ll_nfolio  = dw_2.Object.defe_numero[1]

SELECT	Count(*)
	INTO	:li_existe
	FROM	dba.RECFRUPROCEE_TRANS
	WHERE	plde_codigo	=	:li_planta
	AND	rfpe_numero	=	:ll_nfolio 
	And   clie_codigo =  :li_cliente
	Using (sqlconec);
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla RECFRUPROCEE_TRANS")
	RETURN True
END IF	
	
IF li_existe > 0 THEN	
	Return True
ELSE
	Return False
END IF	
end function

public function boolean coneccionbase ();String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta


DISCONNECT USING sqlconec2;

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

  SELECT cone_nomodb,cone_nomser,cone_nombas,
         cone_nodbms,cone_nomusu,cone_passwo  
    INTO :ls_nomodb,:ls_nomser,:ls_nombas,
	      :ls_nodbms,:ls_Usuario,:ls_Password
    FROM dba.prodconectividad   
   WHERE cone_codigo = 90;

sqlconec2.ServerName	=	ls_nomser
sqlconec2.DataBase	   =	ls_nombas
sqlconec2.Dbms			= 	ls_nodbms
sqlconec2.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'

CONNECT USING sqlconec2;

IF sqlconec2.SQLCode = 0 THEN
	ib_Conectado2	=	True
ELSE
	ib_Conectado2	=	False
END IF

RETURN ib_Conectado2

end function

event open;/*
	Argumentos	:	[1]	=	Código de Planta
						[2]	=	Número de Folio Despacho
						[3]	=	Código de Cliente
						[4]	=	Cantidad de Tarjas
						[5]	=	Código de Embarque
						[7]	=	Código de Destino
						[8]	=	Cantidad de Cajas
*/

/* Si Código de parempresa es igual a 1 se efectua la transacción para REBAJE DE EXISTENCIA*/
  SELECT empr_coacce
    INTO :ii_controlaaceso  
    FROM dba.parempresa  
	 USING sqlca;

istr_mant.argumento[8] =" "
x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

IF gi_CodExport = 300 THEN dw_2.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw				= dw_1
istr_mant.solo_consulta = False

dw_2.GetChild("plde_codigo", dw_planta)
dw_2.GetChild("defe_plades", dw_plades)
dw_2.GetChild("puer_codigo", dw_puerto)
dw_2.GetChild("sire_codigo", dw_sitios)

dw_planta.SetTransObject(sqlca)
dw_plades.SetTransObject(sqlca)
dw_puerto.SetTransObject(sqlca)
dw_sitios.SetTransObject(sqlca)

dw_planta.Retrieve(1)
dw_plades.Retrieve(1)
dw_puerto.Retrieve(1)
dw_sitios.Retrieve(1)

dw_2.SetItem(1, "clie_codigo",gi_codexport)
istr_mant.argumento[1]= String(gi_codplanta)
istr_mant.argumento[3]= String(gi_codexport)
istr_mant.argumento[7]=''
istr_mant.argumento[27]='7'

pb_nuevo.PostEvent(Clicked!)

buscar	= "Código:Nvari_codigo,Descripción:Svari_nombre"
ordenar	= "Código:vari_codigo,Descripción:vari_nombre"
GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
							
sqlconec						=	CREATE Transaction
sqlconec2					=	CREATE Transaction

IF ii_controlaaceso = 1 THEN
	IF NOT coneccionbase() THEN
		MessageBox("Sin Conexión", "No Existe Conexión a Base Control de Acceso.", StopSign!, Ok!)	
	ELSE
		dw_2.Object.defe_patent.Dddw.Name				=	'dw_mues_controlacceso'
		dw_2.Object.defe_patent.Dddw.DisplayColumn	=	'ctac_patent'
		dw_2.Object.defe_patent.Dddw.DataColumn		=	'ctac_patent'
		dw_2.Object.defe_patent.Dddw.AllowEdit			=  True
		dw_2.Object.defe_patent.Dddw.HScrollBar		=  True
		dw_2.Object.defe_patent.Dddw.VScrollBar		=  True
		dw_2.Object.defe_patent.Dddw.Case 				= 	"Upper"
		dw_2.Modify("defe_patent.dddw.Limit=10")
		dw_2.Modify("defe_patent.Dddw.PercentWidth=250")
	
		dw_2.GetChild("defe_patent", idwc_patente)
		idwc_patente.SetTransObject(sqlconec2)
		idwc_patente.Retrieve()
	END IF
END IF

dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_9.SetTransObject(sqlca)
dw_10.SetTransObject(sqlca)

dw_palletfrutahisto.SetTransObject(sqlca)
dw_palletencahisto.SetTransObject(sqlca)
dw_alpalletfruta.SetTransObject(sqlca)
dw_alpalletencab.SetTransObject(sqlca)

iuo_patente				=	CREATE	uo_patente

// Para guardar fecha de despacho en la Palletfruta
ids_palletfruta_fecha				=	CREATE	DataStore
ids_palletfruta_fecha.DataObject	=	'dw_mues_palletfruta_pafrfecdes'
ids_palletfruta_fecha.SetTransObject(sqlca)

ids_CorrelMovim						=	CREATE	DataStore
ids_CorrelMovim.DataObject			=	'dw_mues_correlmoviemientos_despa'
ids_CorrelMovim.SetTransObject(sqlca)

iuo_calibre   =	Create uo_calibre

dw_14.SetTransObject(sqlca)

str_busqueda lstr_busq

lstr_busq					= 	Message.PowerobjectParm
istr_mant.argumento[1]	=	lstr_busq.argum[1]
istr_mant.argumento[2]	=	lstr_busq.argum[2]
istr_mant.argumento[3]	=	lstr_busq.argum[3]

This.TriggerEvent("ue_recuperadatos")

end event

event ue_borra_detalle;call super::ue_borra_detalle;IF dw_1.rowcount() < 2 THEN
	dw_1.SetFocus()
	RETURN
END IF

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_1.RowCount() = 0 THEN 
		HabilitaEncab(True)
		pb_eli_det.Enabled = False
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;istr_mant.borra	= False
istr_mant.agrega	= True

Integer	li_cajas

IF dw_1.RowCount() >= dw_2.Object.defe_cantar[1] THEN
	MessageBox("Atención", "No puede ingresar más Pallets.")
ELSE
	IF dw_1.RowCount() > 0 THEN
		li_cajas	=	dw_1.Object.totcajas[1]
		IF dw_1.Object.totcajas[1] >= Integer(istr_mant.Argumento[8]) THEN
			MessageBox("Atención", "Se ha completado la cantidad de Cajas, No podrá ingresar más Pallets.")
			RETURN
		END IF
	END IF
	
	OpenWithParm(iw_mantencion, istr_mant)
	
	IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
	
	IF dw_1.RowCount() > 0 AND pb_eliminar.Enabled = FALSE THEN
		pb_eliminar.Enabled	= TRUE
		pb_grabar.Enabled		= TRUE
	END IF
	
	dw_1.SetRow(il_fila)
	dw_1.SelectRow(il_fila,True)
END IF
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[3]), &
										Integer(istr_mant.argumento[1]), &
										Long(istr_mant.argumento[2]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				pb_imprimir.Enabled	= True
				IF 	dw_2.Object.defe_estado[1]	=	1	THEN
					istr_mant.solo_consulta	=	True
				ELSE
					istr_mant.solo_consulta 	=	False
					pb_eli_det.Enabled		=	True
					pb_ins_det.Enabled		=	True
					pb_grabar.Enabled		=	True
					pb_eliminar.Enabled		=	True
				END IF
								
				IF ll_fila_d > 0 THEN
					pb_imprimir.Enabled	= True
					
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
				ELSE
					IF 	dw_2.Object.defe_estado[1]	=	1 THEN pb_ins_det.Enabled	=	True							
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)

end event

on w_cons_maed_despafrigoen.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_5=create dw_5
this.dw_6=create dw_6
this.dw_7=create dw_7
this.dw_8=create dw_8
this.dw_9=create dw_9
this.dw_10=create dw_10
this.dw_11=create dw_11
this.dw_12=create dw_12
this.dw_13=create dw_13
this.dw_14=create dw_14
this.dw_15=create dw_15
this.dw_alpalletencab=create dw_alpalletencab
this.dw_alpalletfruta=create dw_alpalletfruta
this.dw_palletencahisto=create dw_palletencahisto
this.dw_palletfrutahisto=create dw_palletfrutahisto
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.dw_4
this.Control[iCurrent+3]=this.dw_5
this.Control[iCurrent+4]=this.dw_6
this.Control[iCurrent+5]=this.dw_7
this.Control[iCurrent+6]=this.dw_8
this.Control[iCurrent+7]=this.dw_9
this.Control[iCurrent+8]=this.dw_10
this.Control[iCurrent+9]=this.dw_11
this.Control[iCurrent+10]=this.dw_12
this.Control[iCurrent+11]=this.dw_13
this.Control[iCurrent+12]=this.dw_14
this.Control[iCurrent+13]=this.dw_15
this.Control[iCurrent+14]=this.dw_alpalletencab
this.Control[iCurrent+15]=this.dw_alpalletfruta
this.Control[iCurrent+16]=this.dw_palletencahisto
this.Control[iCurrent+17]=this.dw_palletfrutahisto
end on

on w_cons_maed_despafrigoen.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.dw_6)
destroy(this.dw_7)
destroy(this.dw_8)
destroy(this.dw_9)
destroy(this.dw_10)
destroy(this.dw_11)
destroy(this.dw_12)
destroy(this.dw_13)
destroy(this.dw_14)
destroy(this.dw_15)
destroy(this.dw_alpalletencab)
destroy(this.dw_alpalletfruta)
destroy(this.dw_palletencahisto)
destroy(this.dw_palletfrutahisto)
end on

event ue_nuevo;HabilitaEncab(True)


ib_ok	= True

CHOOSE CASE wf_modifica()
	CASE -1
		ib_ok = False
	//CASE 0
//		CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
////			CASE 1
////				Message.DoubleParm = 0
////				This.TriggerEvent("ue_guardar")
////				IF message.DoubleParm = -1 THEN ib_ok = False
////			CASE 3
////				ib_ok	= False
////				RETURN
//		END CHOOSE
END CHOOSE

IF ib_ok = False THEN RETURN

dw_1.Reset()
dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()
dw_7.Reset()
dw_8.Reset()
dw_9.Reset()
dw_10.Reset()
dw_11.Reset()
dw_12.Reset()

pb_eli_det.Enabled		= False
pb_ins_det.Enabled		= False
pb_grabar.Enabled			= False
pb_eliminar.Enabled		= False
pb_imprimir.Enabled		= False
istr_mant.solo_consulta	= False
dw_2.Enabled				= True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_codexport)
dw_2.SetItem(1, "plde_codigo", gi_codplanta)
dw_2.SetItem(1,"defe_horade", Now())

dw_2.SetRedraw(True)

dw_2.SetFocus()
end event

event ue_seleccion;call super::ue_seleccion;Str_busqueda			lstr_busq

lstr_busq.argum[1]	= 	istr_mant.argumento[3]
lstr_busq.argum[2]	= 	istr_mant.argumento[1]

OpenWithParm(w_busc_despafrigoen, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[5] <> "" THEN
	istr_mant.argumento[1]	= lstr_busq.argum[2]
	istr_mant.argumento[2]	= lstr_busq.argum[5]
	istr_mant.argumento[3]  = lstr_busq.argum[1]
	ib_existe_folioD			=	True
	This.TriggerEvent("ue_recuperadatos")
	istr_mant.argumento[8]	=	String(dw_2.Object.defe_cancaj[1])
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega			= False
	istr_mant.borra			= False
	istr_mant.Argumento[6]	=	String(dw_1.Object.paen_numero[il_fila])

	//OpenWithParm(w_maed_palletencab_consulta, istr_mant)
	
	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long ll_filas, ll_fila_d,ll_fila_g, ll_nuevofolio, li_fillas

IF dw_2.GetNextModified(0, Primary!) > 0 THEN
	dw_2.SetItem(1, "defe_fecact", Today())
	dw_2.SetItem(1, "defe_horact", Now())
//	IF (dw_1.Object.totcajas[1])<> dw_2.Object.defe_cancaj[1]  THEN
//  		MessageBox("Error de Consistencia", "Cajas No Corresponden con Detalle.", StopSign!, Ok!)
//		dw_2.SetColumn("defe_cancaj")
//		Message.DoubleParm = -1
//	END IF
END IF

ids_palletfruta_fecha.Reset() 
dw_6.Reset()

IF dw_2.Object.defe_tiposa[1] <> 11  THEN
	For ll_filas	=	1	To	dw_1.RowCount()
		IF	dw_6.Retrieve(dw_2.Object.clie_codigo[1],dw_1.Object.paen_numero[ll_filas],dw_2.Object.plde_codigo[1]) > 0 THEN
			FOR ll_fila_d = 1 TO dw_6.RowCount()
				ll_fila_g	=	ids_palletfruta_fecha.InsertRow(0)
				ids_palletfruta_fecha.SetItem(ll_fila_g,'clie_codigo',dw_6.Object.clie_codigo[ll_fila_d])				
				ids_palletfruta_fecha.SetItem(ll_fila_g,'paen_numero',dw_6.Object.paen_numero[ll_fila_d])						
				ids_palletfruta_fecha.SetItem(ll_fila_g,'espe_codigo',dw_6.Object.espe_codigo[ll_fila_d])				
				ids_palletfruta_fecha.SetItem(ll_fila_g,'vari_codigo',dw_6.Object.vari_codigo[ll_fila_d])				
				ids_palletfruta_fecha.SetItem(ll_fila_g,'emba_codigo',dw_6.Object.emba_codigo[ll_fila_d])						
				ids_palletfruta_fecha.SetItem(ll_fila_g,'prod_codigo',dw_6.Object.prod_codigo[ll_fila_d])				
				ids_palletfruta_fecha.SetItem(ll_fila_g,'cond_codigo',dw_6.Object.cond_codigo[ll_fila_d])				
				ids_palletfruta_fecha.SetItem(ll_fila_g,'etiq_codigo',dw_6.Object.etiq_codigo[ll_fila_d])						
				ids_palletfruta_fecha.SetItem(ll_fila_g,'plde_codigo',dw_6.Object.plde_codigo[ll_fila_d])				
				ids_palletfruta_fecha.SetItem(ll_fila_g,'pafr_calibr',dw_6.Object.pafr_calibr[ll_fila_d])				
				ids_palletfruta_fecha.SetItem(ll_fila_g,'pafr_secuen',dw_6.Object.pafr_secuen[ll_fila_d])		
				ids_palletfruta_fecha.SetItem(ll_fila_g,'pafr_fecdes',dw_2.Object.defe_fecdes[1])				
			NEXT
		END IF
	NEXT
	//dw_6.Reset()
	
	//DwItemStatus	Estadol 
	ids_palletfruta_fecha.ResetUpdate()
	
	FOR ll_filas 	=	1 TO 	ids_palletfruta_fecha.RowCount()
		//Estadol	=	ids_palletfruta_fecha.GetItemStatus(ll_filas, 0, Primary!)
		//ids_palletfruta_fecha.SetItemStatus(ll_filas,0,  Primary!, DataModified!	)	
		ids_palletfruta_fecha.SetItemStatus(ll_filas,'pafr_fecdes',  Primary!, DataModified!	)	
	NEXT
END IF

dw_6.Reset()

IF Not ib_existe_folioD	THEN
	IF isnull(dw_2.Object.defe_numero[1]) OR dw_2.Object.defe_numero[1] = 0 THEN
		ll_nuevofolio=BusnuevoFolioDespa(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]))
		il_Folio	=	ll_nuevofolio
		
		IF il_Folio > 0 THEN
			dw_2.Object.defe_numero[1]	= ll_nuevofolio
			dw_2.SetItem(1, "defe_numero",ll_nuevofolio)
		
			istr_mant.argumento[2]	= String(ll_nuevofolio)
			
			FOR li_fillas = 1 TO dw_1.RowCount()
				 dw_1.Object.defe_numero[li_fillas]	= ll_nuevofolio
			NEXT		
		END IF
	END IF	
ELSE
	il_Folio	=	dw_2.Object.defe_numero[1]
END IF
	


end event

event ue_guardar;Integer li_codigo

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 OR il_Folio	=	0 THEN RETURN

	IF wf_actualiza_db(False) THEN
		w_main.SetMicroHelp("Información Grabada.")
		pb_eliminar.Enabled	= True
		pb_imprimir.Enabled	= True
		TriggerEvent("ue_despuesgrabar")
	ELSE
		w_main.SetMicroHelp("No se puede Grabar información.")
		Message.DoubleParm = -1
		RETURN
	END IF 
	
IF dw_2.Object.defe_tiposa[1] = 11 THEN
	
	SELECT empr_codtra  
    INTO :li_codigo  
    FROM dba.parempresa  ;
	 
	IF li_codigo = 1 THEN
   	Message.DoubleParm = 0
		IF ConexionBase() THEN
		   //Existe_CargaRegistro()
			TriggerEvent("ue_despuesguardar")
			IF Message.DoubleParm = -1 OR il_Folio	=	0 THEN RETURN
			
			IF wf_actualiza_Carga(False) THEN
			   Messagebox("Atención","Traspaso de Despacho a Planta Seleccionada Realizada En Forma Satisfactoria",exclamation!) 
				w_main.SetMicroHelp("Información Grabada.")
			ELSE
				Messagebox("Atención","No se pudo transmitir Despacho a Planta Selecionada, Se Generará Archvo Plano",exclamation!) 
				w_main.SetMicroHelp("No se puede Grabar información.")
				GeneraArchivoPlano()
			END IF
		 ELSE
			MessageBox("Sin Conexión", "No Existe Conexión a Zona, Se Procedera a Generar Archivo Plano", StopSign!, Ok!)	
   		GeneraArchivoPlano()
		 END IF
	 ELSE
		Messagebox("Atención","No se pudo transmitir Despacho a Planta Selecionada, Se Generará Archivo Plano",exclamation!) 
	   GeneraArchivoPlano() 	
	 END IF
	
END IF
end event

event ue_listo;IF dw_1.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
	
	IF dw_2.Object.defe_estado[1]	<>	1	THEN	pb_Eliminar.Enabled	=	True
	
	IF istr_mant.Solo_Consulta THEN
		dw_2.Enabled			=	False
//		pb_Eliminar.Enabled	=	False
		pb_Grabar.Enabled		=	False
		pb_ins_det.Enabled	=	False
		pb_eli_det.Enabled	=	False
	ELSE
		dw_2.Enabled			=	True
		pb_Eliminar.Enabled	=	True
		pb_Grabar.Enabled		=	True
		pb_ins_det.Enabled	=	True
		pb_eli_det.Enabled	=	True
	END IF
ELSE
	IF istr_mant.Solo_Consulta THEN
		pb_ins_det.Enabled	=	False
	ELSE
		pb_ins_det.Enabled	=	True
	END IF
END IF

w_main.SetMicroHelp("Listo")

SetPointer(Arrow!)
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_cons_maed_despafrigoen
integer x = 18
integer y = 1188
integer width = 3003
integer height = 948
integer taborder = 100
string title = "Detalle de Pallets"
string dataobject = "dw_mues_despafrigode"
boolean livescroll = false
end type

event dw_1::retrieveend;If RowCount() > 0 Then	pb_ins_det.Enabled = TRUE
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_cons_maed_despafrigoen
integer x = 41
integer y = 32
integer width = 2930
integer height = 1152
string dataobject = "dw_mant_despafrigoen"
boolean livescroll = true
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null
String	ls_columna, ls_null, ls_embq_codigo
Date		ld_nula

SetNull(ll_null)
SetNull(ls_null)
SetNull(ld_nula)
ls_columna = dwo.Name

CHOOSE CASE ls_columna
	CASE "clie_codigo"
		istr_mant.argumento[3]	= data
		IF F_ValidaCliente(Integer(data)) = False THEN
			dw_2.SetItem(il_fila, "clie_codigo", gi_codexport)
			RETURN 1
		ELSE
			dw_2.GetChild("plde_codigo", dw_planta)
			dw_planta.SetTransObject(sqlca)
			dw_planta.Retrieve(1)
			
			dw_2.SetItem(1, "embq_nomnav", ls_null)
			dw_2.SetItem(1, "embq_codigo", ls_null)
			
			dw_2.GetChild("defe_plades", dw_plades)
			dw_plades.SetTransObject(sqlca)
			dw_plades.Retrieve(1)
		END IF
					
	CASE "plde_codigo"
		ExisteFolio(ls_columna, data)
		dw_2.GetChild("defe_plades", dw_plades)
		dw_plades.SetTransObject(sqlca)
		dw_plades.Retrieve(1)
		
	CASE "defe_numero"
		//ExisteFolio(ls_columna, data)
		IF ExisteFolio(ls_columna, data) THEN
			This.SetItem(1, ls_columna, Integer(ls_null))
			RETURN 1
		END IF
		
	CASE "defe_cantar"
		istr_mant.argumento[4]	= data
		
	CASE "defe_cancaj"
		istr_mant.argumento[8]	= data
		
	CASE "embq_codigo"
		IF NoExisteEmbarque(data) THEN
			This.Object.embq_nomnav[row]	=	""
			This.Object.embq_codigo[row]	=	ls_null
			RETURN 1
		END IF

	CASE "defe_tiposa"
		istr_mant.argumento[27] = data
	CASE "tica_codigo"
		

	CASE "defe_fecdes"
		IF Not f_validafechatempo(date(data)) THEN
			This.SetItem(Row, ls_Columna, ld_nula)
			RETURN 1
		END IF	
	
CASE "defe_plasag"
//		IF Not Isnull(data) THEN
//			dw_2.SetTabOrder("sire_codigo",160)
//			dw_2.Modify("sire_codigo.BackGround.Color = " + String(rgb(255,255,255)))
//			dw_2.SetColumn("sire_codigo")
//			dw_2.SetFocus()	
//		ELSE
//			dw_2.SetTabOrder("sire_codigo",0)
//			dw_2.Modify("sire_codigo.BackGround.Color = " + String(RGB(166,180,210)))
//		END IF

CASE "defe_patent"
		IF ib_conectado2 = True THEN
			IF iuo_patente.existe(data,True,sqlconec2) THEN
				dw_2.Object.defe_fecing[Row] = iuo_patente.FechaIng
				dw_2.Object.defe_horing[Row] = iuo_patente.HoraIng
				dw_2.Object.defe_sucuco[Row] = iuo_patente.Sucursal
			ELSE	
				dw_2.Object.defe_fecing[Row] = Date(ls_null)
				dw_2.Object.defe_horing[Row] = Time(ls_null)
				dw_2.Object.defe_sucuco[Row] = Integer(ls_null)
			END IF	
		END IF	
END CHOOSE

HabilitaIngreso(ls_columna)
end event

event dw_2::doubleclicked;//
end event

event dw_2::clicked;call super::clicked;CHOOSE CASE dwo.name
		
	CASE "buscaembarque"
		buscaembarque()
		
END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_cons_maed_despafrigoen
integer x = 3150
integer y = 284
end type

event pb_nuevo::clicked;call super::clicked;ib_existe_folioD	=	False
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_cons_maed_despafrigoen
integer x = 3150
integer y = 512
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_cons_maed_despafrigoen
integer x = 3150
integer y = 728
end type

event pb_grabar::clicked;call super::clicked;//ib_existe_folioD	=	True
end event

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_cons_maed_despafrigoen
integer x = 3150
integer y = 960
boolean enabled = true
boolean default = true
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_cons_maed_despafrigoen
integer x = 3150
integer y = 1188
end type

event pb_salir::clicked;DISCONNECT USING sqlconec2;

Close(Parent)
end event

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_cons_maed_despafrigoen
integer x = 3150
integer y = 1516
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_cons_maed_despafrigoen
integer x = 3150
integer y = 1692
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_cons_maed_despafrigoen
integer x = 3150
integer y = 104
end type

event pb_buscar::clicked;call super::clicked;dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()
dw_7.Reset()
dw_8.Reset()
dw_9.Reset()
dw_10.Reset()
dw_11.Reset()
dw_12.Reset()
end event

type dw_3 from datawindow within w_cons_maed_despafrigoen
boolean visible = false
integer x = 46
integer y = 2212
integer width = 178
integer height = 116
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_recfruprocee_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_cons_maed_despafrigoen
boolean visible = false
integer x = 489
integer y = 2084
integer width = 2405
integer height = 624
integer taborder = 110
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_recfruproced_trans"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within w_cons_maed_despafrigoen
boolean visible = false
integer x = 2825
integer y = 2636
integer width = 663
integer height = 248
integer taborder = 120
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mant_palletencab_historia"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_6 from datawindow within w_cons_maed_despafrigoen
boolean visible = false
integer x = 471
integer y = 2628
integer width = 2094
integer height = 664
integer taborder = 130
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_palletfruta"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_7 from datawindow within w_cons_maed_despafrigoen
boolean visible = false
integer x = 823
integer y = 2204
integer width = 178
integer height = 116
integer taborder = 120
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mant_palletencab_trans"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_8 from datawindow within w_cons_maed_despafrigoen
boolean visible = false
integer x = 1042
integer y = 2220
integer width = 178
integer height = 116
integer taborder = 130
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_palletfruta_trans"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_9 from datawindow within w_cons_maed_despafrigoen
boolean visible = false
integer x = 1253
integer y = 2212
integer width = 178
integer height = 116
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_recupera_palletinspec"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_10 from datawindow within w_cons_maed_despafrigoen
boolean visible = false
integer x = 1454
integer y = 2212
integer width = 178
integer height = 116
integer taborder = 130
boolean bringtotop = true
string title = "none"
string dataobject = "dw_recupera_palletinspecdet"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_11 from datawindow within w_cons_maed_despafrigoen
boolean visible = false
integer x = 1655
integer y = 2212
integer width = 178
integer height = 116
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dw_recupera_palletinspec_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_12 from datawindow within w_cons_maed_despafrigoen
boolean visible = false
integer x = 1856
integer y = 2212
integer width = 178
integer height = 116
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dw_recupera_palletinspecdet_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_13 from datawindow within w_cons_maed_despafrigoen
boolean visible = false
integer x = 2085
integer y = 2216
integer width = 165
integer height = 112
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_gene_archivo_saam_plano"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_14 from datawindow within w_cons_maed_despafrigoen
boolean visible = false
integer x = 2683
integer y = 2160
integer width = 306
integer height = 408
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_cajasprod"
boolean resizable = true
boolean livescroll = true
end type

type dw_15 from datawindow within w_cons_maed_despafrigoen
boolean visible = false
integer x = 2427
integer y = 2164
integer width = 233
integer height = 400
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_cajasprod_trans"
boolean livescroll = true
end type

type dw_alpalletencab from datawindow within w_cons_maed_despafrigoen
boolean visible = false
integer x = 1275
integer y = 2412
integer width = 686
integer height = 400
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_alpalletencab"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_alpalletfruta from datawindow within w_cons_maed_despafrigoen
boolean visible = false
integer x = 2158
integer y = 2428
integer width = 686
integer height = 400
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_alpalletfruta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_palletencahisto from datawindow within w_cons_maed_despafrigoen
boolean visible = false
integer x = 1266
integer y = 2124
integer width = 686
integer height = 400
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_palletencabhisto_inpe"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_palletfrutahisto from datawindow within w_cons_maed_despafrigoen
boolean visible = false
integer x = 2222
integer y = 2128
integer width = 686
integer height = 400
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_palletfrutahisto_inpe"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

