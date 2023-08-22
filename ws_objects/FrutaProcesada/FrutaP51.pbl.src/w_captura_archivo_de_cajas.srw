$PBExportHeader$w_captura_archivo_de_cajas.srw
$PBExportComments$Ventana que captura archivo plano de cajas
forward
global type w_captura_archivo_de_cajas from w_mant_encab_deta
end type
type pb_archivo from picturebutton within w_captura_archivo_de_cajas
end type
type dw_3 from datawindow within w_captura_archivo_de_cajas
end type
type dw_4 from datawindow within w_captura_archivo_de_cajas
end type
type dw_5 from datawindow within w_captura_archivo_de_cajas
end type
type dw_6 from datawindow within w_captura_archivo_de_cajas
end type
end forward

global type w_captura_archivo_de_cajas from w_mant_encab_deta
integer width = 2930
integer height = 1584
string title = "Captura Archivo de Cajas"
string menuname = ""
event ue_carga_cajas ( )
event ue_validaregistro ( )
pb_archivo pb_archivo
dw_3 dw_3
dw_4 dw_4
dw_5 dw_5
dw_6 dw_6
end type
global w_captura_archivo_de_cajas w_captura_archivo_de_cajas

type variables
String             is_Archivo, is_mensaje

OleObject	loo_excel

w_mant_deta_captura_archivocajas  iw_mantencion		 

uo_plantadesp      iuo_plantadesp
uo_especie         iuo_especies
uo_etiquetas       iuo_etiquetas
uo_variedades      iuo_variedades	
//uo_categorias      iuo_categorias
uo_productores     iuo_productores
uo_Prodpredio          iuo_predio
uo_prodcuarteles     iuo_prodcuartel
uo_embalajesprod   iuo_embalajesprod
uo_cliente         iuo_cliente
uo_calibre         iuo_calibre
uo_tipopallet		 iuo_tipopallet

DatawindowChild    idwc_cliente, idwc_planta, idwc_especie, idwc_etiqueta, idwc_variedad, &
		idwc_categoria, idwc_productor, idwc_predio, idwc_cuartel





end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso ()
public function integer buscavarrelacionada (integer ai_especie, integer ai_variedad)
public function string buscaprodrotulado (integer ai_cliente, long al_productor)
protected function boolean wf_actualiza_db (boolean borrando)
public function long buscanumero (integer ai_planta)
public function long buscanuevofolio (integer cliente, integer planta)
public function boolean existecaja (integer ai_cliente, integer ai_planta, long al_caja, boolean ab_mensaje)
end prototypes

event ue_carga_cajas();w_main.SetMicroHelp("Cargando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

Integer     li_planta, li_cliente, li_predio, li_cuartel, li_especie, li_variedad, li_varirotu, li_error, &
            li_categoria, li_etiqueta, li_seleccio, li_embalado, li_pesadora, li_packespe, li_retorno, li_condicion,&
				li_varrotulada, li_plantaori, li_capr_embala
Long        ll_guia, ll_pallet, ll_caja, ll_productor, ll_filas, ll_fila, ll_secuen, ll_filencab,&
				ll_find_enca, ll_find_deta, ll_filfruta, ll_filrecepenca, ll_find_rece, ll_find_tarja,&
				ll_filrecepdeta, ll_numero, ll_docrel
Date        ld_mden_fecdre,ld_fecemb, ld_fecharec, ld_fechaing
String      ls_mensaje, ls_registro, ls_embalaje, ls_calibre, ls_productrotulado, ls_fecha
Time			It_horaing


IF dw_1.RowCount() < 0 THEN
	li_Retorno				= MessageBox("Error","Archivo no pudo ser abierto.", Exclamation!, &
												RetryCancel!)
	Message.DoubleParm	= li_Retorno
ELSEIF dw_1.RowCount() = 0 THEN
		MessageBox("Atención", "Archivo no tiene Filas.")
		Message.DoubleParm	=	2
	 ELSE
	   SetPointer(HourGlass!)
END IF
dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()

FOR ll_Filas = 1 TO dw_1.RowCount()
	
	li_planta    = Integer(istr_Mant.Argumento[1])
	ll_guia      = dw_1.Object.capr_numgia[ll_filas]
	li_cliente   = dw_1.Object.clie_codigo[ll_filas]
	ll_pallet    = dw_1.Object.capr_numpal[ll_filas]
	ll_caja      = dw_1.Object.capr_numero[ll_filas]
	li_plantaori = dw_1.Object.capr_cespak[ll_filas]  
	
	IF  ExisteCaja(li_cliente,li_planta,ll_caja, False) THEN
		EXIT
	ELSE		
		ll_productor 	= dw_1.Object.prod_codigo[ll_filas]
		li_predio    	= dw_1.Object.prod_huerto[ll_filas]
		li_cuartel   	= dw_1.Object.prod_cuarte[ll_filas]
		li_especie   	= dw_1.Object.espe_codigo[ll_filas]
		li_variedad  	= dw_1.Object.capr_varrot[ll_filas]
		li_varirotu  	= dw_1.Object.plde_codigo[ll_filas]
		ls_embalaje  	= dw_1.Object.emba_codigo[ll_filas]
		ld_fecemb    	= dw_1.Object.capr_fecemb[ll_filas]
		li_categoria 	= dw_1.Object.cate_codigo[ll_filas]
		li_etiqueta  	= dw_1.Object.etiq_codigo[ll_filas]
		ls_calibre   	= dw_1.Object.capr_calibr[ll_filas]
		li_packespe  	= dw_1.Object.capr_cespak[ll_filas]
		ll_docrel    	= dw_1.Object.capr_docrel[ll_filas]
		It_horaing	 	= dw_1.Object.capr_hordig[ll_filas]
		ld_fechaing	 	= dw_1.Object.capr_fecdig[ll_filas]
		li_capr_embala	= dw_1.Object.capr_embala[ll_filas]
		
		IF IsNull(li_categoria) THEN li_categoria = 1			
				
		/*
		Llena palletencab
		*/
		ll_find_enca	=	dw_3.Find ( "clie_codigo = " + String(li_cliente) + &
								" AND paen_numero = " + String(ll_pallet) + &
								" AND plde_codigo = " + String(li_planta), 1, dw_3.RowCount() )
								
		IF ll_find_enca > 0 THEN
			dw_3.Object.paen_ccajas[ll_find_enca] = dw_3.Object.paen_ccajas[ll_find_enca] + 1	
			IF li_capr_embala = -1 THEN 
				dw_3.Object.tpem_codigo[ll_find_enca] = String(dw_3.Object.paen_ccajas[ll_find_enca])
			END IF
		ELSE	
			ll_filencab =	dw_3.InsertRow(0)
			
			dw_3.Object.clie_codigo[ll_filencab] = li_cliente
			dw_3.Object.plde_codigo[ll_filencab] = li_planta
			dw_3.Object.paen_numero[ll_filencab] = ll_pallet
			dw_3.Object.espe_codigo[ll_filencab] = li_especie
			dw_3.Object.vari_codigo[ll_filencab] = li_variedad
			dw_3.Object.cate_codigo[ll_filencab] = li_categoria
			dw_3.Object.etiq_codigo[ll_filencab] = li_etiqueta
			dw_3.Object.emba_codigo[ll_filencab] = ls_embalaje
			dw_3.Object.paen_fecemb[ll_filencab] = ld_fecemb
			dw_3.Object.paen_ccajas[ll_filencab] = 1	
			dw_3.Object.paen_estado[ll_filencab] = 1
			dw_3.Object.prod_codigo[ll_filencab] = ll_productor
			dw_3.Object.paen_calibr[ll_filencab] = ls_calibre
			dw_3.Object.paen_huert1[ll_filencab] = li_predio
			dw_3.Object.paen_cuart1[ll_filencab] = li_cuartel
			dw_3.Object.trat_codigo[ll_filencab] = 1
			dw_3.Object.paen_inspec[ll_filencab] = 0
			dw_3.Object.dest_codigo[ll_filencab] = 999
			dw_3.Object.tmvp_codigo[ll_filencab] = 1
			dw_3.Object.cama_codigo[ll_filencab] = 0
			dw_3.Object.frio_codigo[ll_filencab] = '1'	
			
			dw_3.Object.cond_codigo[ll_filencab] = 0
			dw_3.Object.paen_calle[ll_filencab]  = 1
			dw_3.Object.paen_base[ll_filencab]   = 1
			dw_3.Object.paen_posici[ll_filencab] = 1
			dw_3.Object.stat_codigo[ll_filencab] = 1
			
			dw_3.Object.paen_tipopa[ll_filencab] = Abs(li_capr_embala)
			IF li_capr_embala = -1 THEN 
				dw_3.Object.tpem_codigo[ll_filencab] = "1"
			END IF
		END IF
		
		/*
		Llena palletfruta 
		*/
		li_condicion = 0		
//		ll_find_deta	=	dw_4.Find ( "clie_codigo = " + String(li_cliente) + &
//						" AND paen_numero = "  + String(ll_pallet) + &
//						" AND espe_codigo = "  + String(li_especie) + &
//						" AND vari_codigo = "  + String(li_variedad) + &
//						" AND emba_codigo = '" + ls_embalaje + "'" + &
//						" AND prod_codigo = "  + String(ll_productor) + &
//						" AND cond_codigo = "  + String(li_condicion) + &
//						" AND etiq_codigo = "  + String(li_etiqueta) + &
//						" AND plde_codigo = "  + String(li_planta) + &
//						" AND pafr_calibr = '" + ls_calibre + "'" + &
//						" AND pafr_secuen = "  + String(ll_caja) , 1, dw_4.RowCount() )
						
		li_varrotulada 	= buscavarrelacionada(li_especie,li_variedad)	
		ls_productrotulado= buscaprodrotulado(li_cliente,ll_productor)
		
//		IF ll_find_deta > 0 THEN
//			dw_4.Object.pafr_ccajas[ll_find_deta] = 1 //dw_4.Object.pafr_ccajas[ll_find_deta] + 1
//		ELSE
			ll_filfruta =	dw_4.InsertRow(0)
			
			dw_4.Object.clie_codigo[ll_filfruta] = li_cliente
			dw_4.Object.plde_codigo[ll_filfruta] = li_planta
			dw_4.Object.paen_numero[ll_filfruta] = ll_pallet
			dw_4.Object.espe_codigo[ll_filfruta] = li_especie
			dw_4.Object.vari_codigo[ll_filfruta] = li_variedad
			dw_4.Object.cond_codigo[ll_filfruta] = li_condicion
			dw_4.Object.etiq_codigo[ll_filfruta] = li_etiqueta
			dw_4.Object.emba_codigo[ll_filfruta] = ls_embalaje
			dw_4.Object.pafr_fecemb[ll_filfruta] = ld_fecemb
			dw_4.Object.pafr_ccajas[ll_filfruta] = 1	
			dw_4.Object.pafr_secuen[ll_filfruta] = ll_caja 			
			dw_4.Object.prod_codigo[ll_filfruta] = ll_productor
			dw_4.Object.pafr_calibr[ll_filfruta] = ls_calibre
			dw_4.Object.pafr_huert1[ll_filfruta] = li_predio
			dw_4.Object.pafr_cuart1[ll_filfruta] = li_cuartel
			dw_4.Object.pafr_copack[ll_filfruta] = li_packespe
			dw_4.Object.pafr_varrot[ll_filfruta] = li_varrotulada
			dw_4.Object.pafr_prdrot[ll_filfruta] = Long(ls_productrotulado)
			dw_4.Object.pafr_cjssal[ll_filfruta] = 0
			dw_4.Object.pafr_fecing[ll_filfruta] = Date(Today())
			dw_4.Object.pafr_ccajas[ll_filfruta]= 1 
//		END IF
		
		/*
		carga recepcion encabezado
		*/
		ld_fecharec = date(today())
		ls_fecha = String(ld_fecharec)
		
		ll_find_rece	=	dw_5.Find ( "plde_codigo = " + String(li_planta)+ &
								" AND String(rfpe_fecrec, 'dd/mm/yyyy') = '" + String(Date(ls_fecha), 'dd/mm/yyyy')+"'", 1, dw_5.RowCount() )
									
		
		IF ll_find_rece = 0 THEN
			//ll_numero = buscanumero(li_planta)
			ll_numero = buscanuevofolio(li_cliente,li_planta) 
			
			ll_filrecepenca =	dw_5.InsertRow(0)
			
			dw_5.Object.clie_codigo[ll_filrecepenca] = li_cliente
			dw_5.Object.plde_codigo[ll_filrecepenca] = li_planta
			dw_5.Object.rfpe_numero[ll_filrecepenca] = ll_numero
			dw_5.Object.rfpe_ptaori[ll_filrecepenca] = li_packespe
			dw_5.Object.rfpe_fecrec[ll_filrecepenca] = Date(Today())
			dw_5.Object.rfpe_guides[ll_filrecepenca] = ll_guia
			dw_5.Object.tpmv_codigo[ll_filrecepenca] = 1
			dw_5.Object.rfpe_tipoen[ll_filrecepenca] = 1
			dw_5.Object.prod_codigo[ll_filrecepenca] = ll_productor
			dw_5.Object.rfpe_horrec[ll_filrecepenca] = Now()
			dw_5.Object.rfpe_fecing[ll_filrecepenca] = ld_fechaing
			dw_5.Object.rfpe_horing[ll_filrecepenca] = It_horaing
		END IF
		
		/*  
		carga recepcion detalle
		*/
		ll_find_tarja	=	dw_6.Find ( "clie_codigo = " + String(li_cliente) + &
						" AND paen_numero = " + String(ll_pallet) + &
						" AND rfpe_numero = " + String(ll_numero) + &
						" AND plde_codigo = " + String(li_planta), 1, dw_6.RowCount() )
		
		IF ll_find_tarja = 0 THEN
			ll_filrecepdeta = dw_6.InsertRow(0)
			
			dw_6.Object.clie_codigo[ll_filrecepdeta] = li_cliente
			dw_6.Object.plde_codigo[ll_filrecepdeta] = li_planta
			dw_6.Object.paen_numero[ll_filrecepdeta] = ll_pallet
			dw_6.Object.rfpe_numero[ll_filrecepdeta] = ll_numero
		END IF
	END IF		
NEXT
SetPointer(Arrow!)
dw_1.SetRedraw(True)

Message.DoubleParm 	= li_retorno

end event

event ue_validaregistro();Integer	li_cont, li_cliente, li_planta, ll_caja
String	ls_mensaje, ls_colu[], ls_calibre, ls_calibre1,ls_Embalaje,ls_tpem_codigo,ls_capr_embala
Long     ll_fila

IF dw_3.RowCount() > 0 THEN
	FOR ll_fila = 1 TO dw_3.RowCount()
		IF dw_3.Object.paen_tipopa[ll_fila] = 1 THEN
			li_cliente   	= dw_3.Object.clie_codigo[ll_fila]
			ls_Embalaje	 	= dw_3.Object.emba_codigo[ll_fila]
			ls_tpem_codigo	= dw_3.Object.tpem_codigo[ll_fila]
			IF NOT iuo_tipopallet.existe(li_cliente,ls_Embalaje,ls_tpem_codigo,False,sqlca) THEN
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nTipo Embalaje "+ ls_tpem_codigo + &
										" Pallet " + STring(dw_3.Object.paen_numero[ll_fila])
				ls_colu[li_cont]= "clie_codigo"	
			END IF
		END IF
	NEXT
END IF

IF dw_1.RowCount() >0 THEN	
   FOR ll_fila = 1 to dw_1.RowCount()
		li_cliente   	= dw_1.Object.clie_codigo[ll_fila]
		li_planta   	= Integer(istr_Mant.Argumento[1])
		ll_caja  	   = dw_1.Object.capr_numero[ll_fila]
		
		IF  ExisteCaja(li_cliente,li_planta,ll_caja, False) THEN
			li_cont ++
			ls_mensaje 		 = ls_mensaje + "~nCaja "+ String(dw_1.Object.capr_numero[ll_fila]) + &
									" Fila : " + String(ll_fila) +" Existe"
			ls_colu[li_cont]= "capr_numero"			
		END IF
		
		IF NOT iuo_cliente.existe(dw_1.Object.clie_codigo[ll_fila],False,sqlca) THEN
			li_cont ++
			ls_mensaje 		 = ls_mensaje + "~nCliente "+ String(dw_1.Object.clie_codigo[ll_fila]) + &
									" Fila : " + String(ll_fila) 
			ls_colu[li_cont]= "clie_codigo"
		END IF
		IF NOT iuo_plantadesp.existe(dw_1.Object.plde_codigo[ll_fila],False,sqlca) THEN
			li_cont ++
			ls_mensaje 		 = ls_mensaje + "~nPacking "+ String(dw_1.Object.plde_codigo[ll_fila]) + &
									" Fila : " + String(ll_fila) 
			ls_colu[li_cont]= "plde_codigo"
		END IF
		IF NOT Isnull(dw_1.Object.prod_codigo[ll_fila]) AND dw_1.Object.prod_codigo[ll_fila] > 0 THEN
			IF NOT iuo_productores.existe(dw_1.Object.prod_codigo[ll_fila],False,sqlca) THEN
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nProductor "+ String(dw_1.Object.prod_codigo[ll_fila]) + &
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "prod_codigo"
			END IF
		END IF
		IF NOT Isnull(dw_1.Object.prod_predio[ll_fila]) AND dw_1.Object.prod_predio[ll_fila] > 0 THEN
			IF NOT iuo_predio.existe(dw_1.Object.prod_predio[ll_fila],dw_1.Object.prod_codigo[ll_fila],False,sqlca) THEN
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nPredio "+ String(dw_1.Object.prod_predio[ll_fila]) + &
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "prod_predio"
			END IF
		END IF
		IF NOT Isnull(dw_1.Object.prod_cuarte[ll_fila]) AND dw_1.Object.prod_cuarte[ll_fila] > 0 THEN
			IF NOT iuo_prodcuartel.existe(dw_1.Object.prod_codigo[ll_fila],dw_1.Object.prod_predio[ll_fila],dw_1.Object.prod_cuarte[ll_fila],False,sqlca) THEN
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nCuartel " + String(dw_1.Object.prod_cuarte[ll_fila]) + &
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "prod_cuarte"	
			END IF
		END IF
		IF NOT Isnull(dw_1.Object.espe_codigo[ll_fila]) AND dw_1.Object.espe_codigo[ll_fila] > 0 THEN
			IF NOT iuo_especies.existe(dw_1.Object.espe_codigo[ll_fila],False,sqlca) THEN
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nEspecie " + String(dw_1.Object.espe_codigo[ll_fila]) + &
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "espe_codigo"	  
			END IF
		END IF
		IF NOT Isnull(dw_1.Object.vari_codigo[ll_fila]) AND dw_1.Object.vari_codigo[ll_fila] > 0 THEN
			IF NOT iuo_variedades.existe(dw_1.Object.espe_codigo[ll_fila],dw_1.Object.vari_codigo[ll_fila],False,sqlca) THEN
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nVariedad " + String(dw_1.Object.vari_codigo[ll_fila]) + &
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "vari_codigo"	   
			END IF
		END IF
		IF NOT Isnull(dw_1.Object.capr_varrot[ll_fila]) AND dw_1.Object.capr_varrot[ll_fila] > 0 THEN
			IF NOT iuo_variedades.existe(dw_1.Object.espe_codigo[ll_fila],dw_1.Object.capr_varrot[ll_fila],False,sqlca) THEN
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nVariedad Rotulada " + String(dw_1.Object.capr_varrot[ll_fila]) + &
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "capr_varrot"	   
			END IF
		END IF
		IF NOT Isnull(dw_1.Object.emba_codigo[ll_fila]) AND dw_1.Object.emba_codigo[ll_fila] <>"" THEN
			IF NOT iuo_embalajesprod.existe(dw_1.Object.clie_codigo[ll_fila],dw_1.Object.emba_codigo[ll_fila],False,sqlca) THEN
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nEmbalaje " + dw_1.Object.emba_codigo[ll_fila] + &
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "emba_codigo"	     
			END IF
		END IF
		IF NOT Isnull(dw_1.Object.etiq_codigo[ll_fila]) AND dw_1.Object.etiq_codigo[ll_fila] > 0  THEN
			IF NOT iuo_etiquetas.existe(dw_1.Object.etiq_codigo[ll_fila],False,sqlca) THEN
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nEtiqueta " + String(dw_1.Object.etiq_codigo[ll_fila]) + &
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "etiq_codigo"	       
			END IF
		END IF	 
		IF NOT Isnull(dw_1.Object.capr_calibr[ll_fila]) AND dw_1.Object.capr_calibr[ll_fila] <> "" THEN
			ls_calibre1 = Trim(dw_1.Object.capr_calibr[ll_fila])
			ls_calibre = ls_calibre1+''+'   '
			ls_calibre = left(ls_calibre,3)
			IF NOT iuo_calibre.existe(dw_1.Object.espe_codigo[ll_fila],dw_1.Object.vari_codigo[ll_fila],ls_calibre,False,sqlca) THEN
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nCalibre " + ls_calibre + & 
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "capr_calibr"	        
			END IF
		END IF
	NEXT
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Código de " + ls_mensaje + " No Existe en tabla respectiva.", StopSign!, Ok!)
	dw_3.Reset()
	dw_4.Reset()
	dw_5.Reset()
	dw_6.Reset()
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
	pb_archivo.SetFocus()
END IF
end event

public subroutine habilitaencab (boolean habilita);//IF Habilita THEN
//	dw_2.Enabled	= True
//	dw_2.Object.clpr_rut.Protect=1
//	dw_2.Object.clpr_nombre.Protect=1
//	dw_2.Object.mden_docrel.Protect=1
//	dw_2.Object.ocen_numero.Protect=1
//	dw_2.Object.reen_chofer.Protect=1
//	dw_2.Object.reen_patent.Protect=1
//	dw_2.Object.reen_dirdes.Protect=1
//	dw_2.Object.mden_fecdre.Protect=1
//	dw_2.SetTabOrder("bode_codigo", 10)
//	dw_2.Modify("bode_codigo.BackGround.Color = " + String(rgb(255,255,255)))
//END IF
//
//RETURN
end subroutine

public subroutine habilitaingreso ();//IF istr_mant.solo_consulta THEN RETURN
//
//Boolean	Habilita	= True
//
//dw_2.AcceptText()
//
//IF Isnull(dw_2.GetItemString(1,"clpr_rut")) THEN
//	Habilita = False
//ELSEIF Isnull(dw_2.GetItemNumber(1,"ocen_numero")) THEN
//	Habilita = False
//ELSEIF Isnull(dw_2.GetItemNumber(1,"mden_docrel")) THEN
//	Habilita = False
//ELSEIF Isnull(dw_2.GetItemDate(1,"mden_fecdre")) THEN
//	Habilita = False
//
//END IF
//
//IF is_nuevo = 'N' THEN
//	Habilita = False
//END IF
//
//pb_ins_det.Enabled	= Habilita
end subroutine

public function integer buscavarrelacionada (integer ai_especie, integer ai_variedad);Integer li_variedad

  SELECT vari_relaci  
    INTO :li_variedad  
    FROM dba.variedades  
   WHERE espe_codigo = :ai_especie  AND  
         vari_codigo = :ai_variedad;


IF sqlca.SQLCode =-1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla variedades")
	
END IF

Return li_variedad




end function

public function string buscaprodrotulado (integer ai_cliente, long al_productor);String ls_productor

  SELECT prpr_codigo  
    INTO :ls_productor  
    FROM dba.productoresprod  
   WHERE clie_codigo = :ai_cliente  AND  
         prod_codigo = :al_productor;


IF sqlca.SQLCode =-1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla productoresprod")
	
END IF

Return ls_productor




end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			IF dw_3.Update(True, False) = 1 THEN
				IF dw_4.Update(True, False) = 1 THEN
					IF dw_5.Update(True, False) = 1 THEN
						IF dw_6.Update(True, False) = 1 THEN
					Commit;
					
							IF sqlca.SQLCode <> 0 THEN
								F_ErrorBaseDatos(sqlca, This.Title)
								
								RollBack;
							ELSE
								lb_Retorno	=	True
								
								dw_1.ResetUpdate()
								dw_2.ResetUpdate()
								dw_3.ResetUpdate()
								dw_4.ResetUpdate()
								dw_5.ResetUpdate()
								dw_6.ResetUpdate()
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
					
					RollBack;
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
		
		RollBack;
	END IF	
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			IF dw_3.Update(True, False) = 1 THEN
				IF dw_4.Update(True, False) = 1 THEN
					IF dw_5.Update(True, False) = 1 THEN
						IF dw_6.Update(True, False) = 1 THEN
							Commit;
							
							IF sqlca.SQLCode <> 0 THEN
								F_ErrorBaseDatos(sqlca, This.Title)
								
								RollBack;
							ELSE
								lb_Retorno	=	True
								
								dw_1.ResetUpdate()
								dw_2.ResetUpdate()
								dw_3.ResetUpdate()
								dw_4.ResetUpdate()
								dw_5.ResetUpdate()
								dw_6.ResetUpdate()
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
					
					RollBack;
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
		
		RollBack;
	END IF	
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function long buscanumero (integer ai_planta);Long ll_numero

  SELECT Max(rfpe_numero)  
    INTO :ll_numero  
    FROM dba.recfruprocee  
   WHERE plde_codigo = :ai_planta;


IF sqlca.SQLCode =-1 THEN
	F_ErrorBaseDatos(sqlca,"Problema para obtener número Recepcion")
END IF

IF Isnull(ll_numero) OR ll_numero = 0 THEN
	MessageBox("Atención","No existe número de correlativo en el mantenedor.",&
							Exclamation!, OK!)
ELSE
	ll_numero = ll_numero + 1 
END IF	

Return ll_numero




end function

public function long buscanuevofolio (integer cliente, integer planta);Integer	li_planta, li_tipoins, li_movto
Long		ll_numero,ll_numero2, ll_fin, ll_actual

li_planta	=	planta

li_movto = 1

SELECT Max(rfpe_numero)
  INTO :ll_numero
  FROM DBA.RECFRUPROCEE
 WHERE plde_codigo = :li_planta;
 
Select como_inicia, como_actual, como_termin
Into	:ll_numero2, :ll_actual, :ll_fin
from DBA.CORRELMOVIMIENTOS 
Where plde_codigo = :li_planta
and	COMO_TIPOMV = :li_movto;

IF ll_actual >= ll_fin THEN
	Return 0
END IF	

ll_fin = ll_fin - 3

IF ll_actual >= ll_fin THEN 
	MessageBox("Advertencia","Quedan menos de 3 Correlativos, Proceda por Mantención 'Correlativos'")
END IF	

IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla CORRELMOVIMIENTOS")
END IF

IF Isnull(ll_numero) OR String(ll_numero) = '' or ll_numero < ll_numero2 THEN
	ll_numero = ll_numero2
END IF	


IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla RecFruProcee")
ELSEIF sqlca.SQLCode = 0 THEN
	ll_numero++
END IF

RETURN ll_numero









end function

public function boolean existecaja (integer ai_cliente, integer ai_planta, long al_caja, boolean ab_mensaje);Long ll_numero

SELECT capr_numero  
  INTO :ll_numero  
  FROM dba.spro_cajasprod
  WHERE clie_codigo = :ai_cliente AND  
        plde_codigo = :ai_planta  AND  
        capr_numero = :al_caja;

IF sqlca.SQLCode =-1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Spro_cajasprod")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	RETURN FALSE
ELSEIf ab_mensaje THEN
	MessageBox("Atención","Archivo de Cajas ya fue Ingresado.",&
							Exclamation!, OK!)
	RETURN TRUE
ELSE
	RETURN TRUE
END IF




end function

on w_captura_archivo_de_cajas.create
int iCurrent
call super::create
this.pb_archivo=create pb_archivo
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_5=create dw_5
this.dw_6=create dw_6
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_archivo
this.Control[iCurrent+2]=this.dw_3
this.Control[iCurrent+3]=this.dw_4
this.Control[iCurrent+4]=this.dw_5
this.Control[iCurrent+5]=this.dw_6
end on

on w_captura_archivo_de_cajas.destroy
call super::destroy
destroy(this.pb_archivo)
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.dw_6)
end on

event open;call super::open;im_menu	= m_principal

This.ParentWindow().ToolBarVisible	= True
im_menu.Item[1].Item[6].Enabled		= True
im_menu.Item[7].Enabled					= True


iuo_plantadesp     = CREATE  uo_plantadesp      
iuo_especies       = CREATE  uo_especie 
iuo_etiquetas      = CREATE  uo_etiquetas       
iuo_variedades	    = CREATE  uo_variedades       
//uo_categorias      iuo_categorias
iuo_productores    = CREATE  uo_productores     
iuo_predio         = CREATE  uo_ProdPredio          
iuo_prodcuartel    = CREATE  uo_prodcuarteles
iuo_embalajesprod  = CREATE  uo_embalajesprod
iuo_cliente        = CREATE  uo_cliente
iuo_calibre        = CREATE  uo_calibre
iuo_tipopallet		 =	CREATE  uo_tipopallet		 

dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)

istr_Mant.Argumento[1]	=	String(gi_CodPlanta)	 

pb_archivo.PostEvent(Clicked!)




end event

event ue_nuevo;call super::ue_nuevo;Long		ll_modif1, ll_modif2

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif1	=	dw_1.GetNextModified(0, Primary!)
			ll_modif2	=	dw_2.GetNextModified(0, Primary!)
		
//			IF ib_ModEncab OR dw_1.GetNextModified(0, Primary!) > 0 THEN
			IF dw_1.RowCount() > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						IF message.DoubleParm = -1 THEN ib_ok = False
					CASE 3
						ib_ok	= False
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF

IF Not ib_ok THEN RETURN

dw_1.Reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.SetFocus()

pb_archivo.Enabled = TRUE




end event

event ue_antesguardar;Long	ll_Fila

This.TriggerEvent("ue_validaregistro")

IF Message.DoubleParm <> -1 THEN	
	FOR ll_Fila = 1 TO dw_1.RowCount()
		dw_1.Object.plde_codigo[ll_Fila] = Integer(istr_Mant.Argumento[1])
	NEXT
END IF


end event

event ue_imprimir;SetPointer(HourGlass!)
Integer	li_Cliente, li_Planta
Long		fila, ll_Pallet
str_info	lstr_info

IF dw_1.RowCount() > 0 THEN
	li_Cliente	= dw_1.Object.clie_codigo[1]
	li_Planta	= dw_1.Object.plde_codigo[1]
	ll_Pallet	= dw_1.Object.capr_numpal[1]
	 
	lstr_info.titulo	= "RECEPCIÓN ARCHIVO DE CAJAS"
	lstr_info.copias	= 1
	
	OpenWithParm(vinf,lstr_info)
	
	vinf.dw_1.DataObject = "dw_info_captura_archivocajas_trans" 	 //dw_info_captura_archivocajas
	vinf.dw_1.SetTransObject(sqlca)
	
	dw_1.ShareData(vinf.dw_1)
	Fila	=	dw_1.RowCount()
	//fila = vinf.dw_1.Retrieve(li_Cliente,li_Planta,ll_Pallet)
	
	IF fila = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
						"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
	ELSEIF fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", &
						StopSign!, Ok!)
	ELSE
		F_Membrete(vinf.dw_1)
		IF dw_5.RowCount() > 0 THEN
			vinf.dw_1.Modify("t_recepcion.text = '"+ String(dw_5.Object.rfpe_numero[1]) + "'")		
		END IF
		
		IF gs_Ambiente <> 'Windows' THEN
			F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
		END IF
	END IF
		
	SetPointer(Arrow!)
END IF
end event

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

IF dw_1.width > il_AnchoDw_1 THEN
	maximo		=	dw_1.width
ELSE
	dw_1.width	=	This.WorkSpaceWidth() - 400
	maximo		=	dw_1.width
END IF
//dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
//dw_2.y					= 37

////dw_1.x					= This.WorkSpaceWidth() - 330
//dw_1.x					= 68
//dw_1.y               = 30
////dw_1.y					= 64 + dw_2.Height
dw_1.height				= This.WorkSpaceHeight() - dw_1.y - 41

li_posic_x				= This.WorkSpaceWidth() - 240
li_posic_y				= 300	

IF pb_buscar.Visible THEN
	pb_buscar.x				= li_posic_x
	pb_buscar.y				= li_posic_y
	pb_buscar.width		= 233
	pb_buscar.height		= 196
	li_visible ++
	li_posic_y += 195
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				= li_posic_x
	pb_nuevo.y				= li_posic_y
	pb_nuevo.width			= 233
	pb_nuevo.height		= 196
	li_visible ++
	li_posic_y += 195
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x				= li_posic_x
	pb_grabar.y				= li_posic_y
	pb_grabar.width		= 233
	pb_grabar.height		= 196
	li_visible ++
	li_posic_y += 195
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			= li_posic_x
	pb_imprimir.y			= li_posic_y
	pb_imprimir.width		= 233
	pb_imprimir.height	= 196
	li_visible ++
	li_posic_y += 195
END IF


IF pb_archivo.Visible THEN
	pb_archivo.x			= li_posic_x
	pb_archivo.y			= li_posic_y
	pb_archivo.width		= 233
	pb_archivo.height		= 196
	li_visible ++
	li_posic_y += 195
END IF

IF	pb_eliminar.Visible THEN
	pb_eliminar.x			= li_posic_x
	pb_eliminar.y			= li_posic_y
	pb_eliminar.width		= 233
	pb_eliminar.height	= 196
	li_visible ++
	li_posic_y += 195
END IF

IF pb_salir.Visible THEN
	pb_salir.x				= li_posic_x
	pb_salir.y				= li_posic_y
	pb_salir.width			= 233
	pb_salir.height		= 196
	li_visible ++
	li_posic_y += 195
END IF

pb_ins_det.x			= li_posic_x
pb_ins_det.y			= 1300
pb_ins_det.width		= 233
pb_ins_det.height		= 196

pb_eli_det.x			= li_posic_x
pb_eli_det.y			= pb_ins_det.y + 195
pb_eli_det.width		= 233
pb_eli_det.height		= 196


end event

event ue_guardar;//
IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")
Message.DoubleParm = 0
TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = 0 THEN
   IF wf_actualiza_db(False) THEN
		pb_imprimir.Enabled = TRUE
		w_main.SetMicroHelp("Información Grabada.")
		MessageBox( "Atención", "El Proceso a Concluído Satisfactoriamente.", &
			Information!, Ok!)
		pb_imprimir.Enabled	= True
   ELSE
	   w_main.SetMicroHelp("No se puede Grabar información.")
	   Message.DoubleParm = -1
		
	   RETURN
	END IF
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
   Message.DoubleParm = -1
   RETURN
END IF
end event

event ue_modifica_detalle;call super::ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_listo;IF dw_1.RowCount() > 0 THEN
	IF istr_mant.Solo_Consulta THEN
		dw_2.Enabled			=	False
   	pb_Grabar.Enabled		=	False
		//pb_ins_det.Enabled	=	False
		//pb_eli_det.Enabled	=	False
	ELSE
		dw_2.Enabled			=	True
		pb_Eliminar.Enabled	=	True
		pb_Grabar.Enabled		=	True
		//pb_ins_det.Enabled	=	True
		//pb_eli_det.Enabled	=	True
	END IF
//ELSE
//	IF istr_mant.Solo_Consulta THEN
//		pb_ins_det.Enabled	=	False
//	ELSE
//		pb_ins_det.Enabled	=	True
//	END IF
END IF

w_main.SetMicroHelp("Listo")

SetPointer(Arrow!)
end event

type dw_1 from w_mant_encab_deta`dw_1 within w_captura_archivo_de_cajas
integer x = 82
integer y = 52
integer width = 2574
integer height = 1348
integer taborder = 100
string title = "Detalle de Captura"
string dataobject = "dw_mues_arch_planos_cajasprod"
end type

type dw_2 from w_mant_encab_deta`dw_2 within w_captura_archivo_de_cajas
boolean visible = false
integer x = 704
integer y = 1188
integer width = 229
integer height = 92
end type

event dw_2::doubleclicked;//
end event

event dw_2::itemfocuschanged;call super::itemfocuschanged;//IF is_rut <> "" THEN
//	IF Len(is_rut) < 10 THEN
//		This.Object.clpr_rut.Format = '@@@@@@'
//	ELSE
//		This.Object.clpr_rut.Format = '@@@.@@@.@@@-@'
//	END IF
//	
//	IF dwo.name <> "clpr_rut" THEN
//		This.SetItem(1, "clpr_rut", is_rut)
//	END IF
//END IF
end event

event dw_2::help;call super::help;//Boolean	lb_AutoCommit, lb_Retorno
//Integer	li_concon
//
//IF Not dw_2.uf_check_required(0) THEN RETURN False
//
//IF Not dw_1.uf_validate(0) THEN RETURN False
//
//lb_AutoCommit		=	sqlca.AutoCommit
//sqlca.AutoCommit	=	False
//
//IF Borrando THEN
//	IF dw_1.Update() = -1 THEN
//		F_ErrorBaseDatos(sqlca, This.Title)
//	 ELSEIF dw_2.Update() = -1 THEN
//		       F_ErrorBaseDatos(sqlca, This.Title)
//			    RollBack;
//			 ELSE
//				// Bloque Grabación
//				SELECT Expa_concon INTO :li_concon
//				FROM dba.Existeparam
//				WHERE expa_identi = 1;
//				UPDATE dba.Existeparam SET
//				expa_concon = li_concon
//				WHERE expa_identi = 1;
//				//
//				
//		       Commit;
//		
//		       IF sqlca.SQLCode <> 0 THEN
//			       F_ErrorBaseDatos(sqlca, This.Title)
//		       ELSE
//			       lb_Retorno	=	True
//		       END IF
//	       END IF
//ELSE
//	IF dw_2.Update() = -1 THEN
//		F_ErrorBaseDatos(sqlca, This.Title)
//	ELSEIF dw_1.Update() = -1 THEN
//		     F_ErrorBaseDatos(sqlca, This.Title)
//		 RollBack;
//	ELSE
//		// Bloque Grabación
//		SELECT Expa_concon INTO :li_concon
//		FROM dba.Existeparam
//		WHERE expa_identi = 1;
//		UPDATE dba.Existeparam SET
//		expa_concon = li_concon
//		WHERE expa_identi = 1;
//		//		
//		Commit;
//		
//		IF sqlca.SQLCode <> 0 THEN
//			F_ErrorBaseDatos(sqlca, This.Title)
//		ELSE
//			lb_Retorno	=	True
//		END IF
//	END IF
//END IF
//
//sqlca.AutoCommit	=	lb_AutoCommit
//
//RETURN lb_Retorno
end event

event dw_2::itemchanged;call super::itemchanged;//String       ls_columna
//Integer      li_null
//
//SetNull(li_null)
//
//ls_columna = dwo.name
//
//CHOOSE CASE ls_columna
//	CASE "bode_codigo"
//		IF NoExisteBodega(Integer(data)) THEN
//		   This.SetItem(il_fila, "bode_codigo", li_null)
//		   RETURN 1
//		ELSE
//			pb_correo.Enabled = TRUE
//	   END IF	
//		
//END CHOOSE		
end event

type pb_nuevo from w_mant_encab_deta`pb_nuevo within w_captura_archivo_de_cajas
integer x = 3264
integer y = 380
alignment htextalign = right!
end type

type pb_eliminar from w_mant_encab_deta`pb_eliminar within w_captura_archivo_de_cajas
boolean visible = false
integer x = 3264
integer y = 1100
string picturename = "\desarrollo\bmp\anulae.bmp"
string disabledname = "\desarrollo\bmp\anulad.bmp"
end type

event pb_eliminar::clicked;//Boolean		lb_mespro
//Date			ld_fecha
//Integer		li_tipmov
//Long			ll_numero, ll_ocurre
//
//
//is_emitir	=  ''
//ld_fecha		=	Date('01/'+Mid(istr_mant.argumento[3],4))
//
//IF  ld_fecha < gstr_param.mes_proceso THEN
//				istr_mant.solo_consulta = True
//				MessageBox("Error de Inconsistencia", "No Puede Eliminar Movimiento con Fecha Anterior al Mes de Proceso.")
//				lb_mespro = True
//ELSE
//				istr_mant.solo_consulta = False
//				lb_mespro = False
//END IF
//
//
//li_tipmov	=	Integer(istr_mant.argumento[1])
//ll_numero	=	Long(istr_mant.argumento[2])
//
//IF li_tipmov =  1 THEN
//	
//			DECLARE revisa_saldos PROCEDURE FOR dba.Exis_RevisaSaldosItemsdeGuia  
//						@tipdoc = :li_tipmov,   
//						@numero = :ll_numero  ;
//			EXECUTE revisa_saldos ;
//			
//			IF SQLCA.SQLCode < 0 THEN
//				MessageBox("Error en Revisión de Saldos", "Se ha producido un Error en Revisión de Saldos.~r~r" + &
//								SQLCA.SQLErrText)
//			ELSE
//			   FETCH revisa_saldos INTO :ll_ocurre ;	// Numero de Documento Generado
//			   CLOSE revisa_saldos;
//			
//				IF ll_ocurre > 0 THEN
//					MessageBox("Saldo Negativo", "No Puede Eliminar Movimiento: Un(os) Item(s) Quedará(n) con Saldo Negativo.")
//				END IF
//				
//			END IF
//ELSE
//		ll_ocurre = 0
//END IF
//	
//
//IF lb_mespro = False AND ll_ocurre =0 THEN
//
//	CALL SUPER::Clicked
//
//END IF
end event

type pb_grabar from w_mant_encab_deta`pb_grabar within w_captura_archivo_de_cajas
integer x = 3264
integer y = 560
end type

type pb_imprimir from w_mant_encab_deta`pb_imprimir within w_captura_archivo_de_cajas
integer x = 3264
integer y = 740
end type

type pb_salir from w_mant_encab_deta`pb_salir within w_captura_archivo_de_cajas
integer x = 3264
integer y = 1280
end type

type pb_ins_det from w_mant_encab_deta`pb_ins_det within w_captura_archivo_de_cajas
boolean visible = false
integer y = 1528
end type

type pb_eli_det from w_mant_encab_deta`pb_eli_det within w_captura_archivo_de_cajas
boolean visible = false
integer y = 1700
end type

type pb_buscar from w_mant_encab_deta`pb_buscar within w_captura_archivo_de_cajas
boolean visible = false
integer x = 3264
integer y = 200
end type

type pb_archivo from picturebutton within w_captura_archivo_de_cajas
integer x = 3264
integer y = 916
integer width = 155
integer height = 132
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\desarrollo\bmp\buscaarc.bmp"
alignment htextalign = left!
end type

event clicked;String	ls_directorio, ls_archivo, path, nombre, pathpordefault
Integer	li_valida, li_opcion = 1
Long		ll_rc
dwitemstatus stat

ib_ok	= True

IF dw_1.AcceptText() = -1 THEN li_opcion = -1
IF dw_1.ModifiedCount() > 0 THEN 
	li_opcion = 0
END IF

CHOOSE CASE li_opcion
	CASE -1
		ib_ok = False
	CASE 0
		CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
			CASE 1
				Message.DoubleParm = 0
				This.triggerevent("ue_guardar")
				IF message.doubleparm = -1 THEN ib_ok = False
				RETURN
			CASE 3
				ib_ok	= False
				RETURN
		END CHOOSE
END CHOOSE

IF ib_ok = False THEN RETURN

dw_1.Reset()

DO
	
RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, pathpordefault)

IF GetFileOpenName ("Carga de Archivo", path, nombre,  "csv", "Archivos Planos (*.csv),*.csv", pathpordefault)< 1 THEN Return

li_valida = dw_1.importfile(path)

	IF li_valida = 0 THEN
		pb_salir.SetFocus()
		dw_1.Insertrow(0)
		RETURN
	ELSEIF li_valida = -1 THEN
		MessageBox("Error de Apertura","Ocurrió un error al utilizar el archivo",Information!,Ok!)
		Message.DoubleParm = 1
	ELSE				
		Message.DoubleParm = 2
		Parent.TriggerEvent("ue_carga_cajas")
						
		IF Message.DoubleParm = 2 THEN 
			MessageBox("Atención", "No se cargó archivo exitosamente," + is_mensaje + ".", StopSign!, Ok!)
		   FileClose(li_valida)
		ELSE			
			//Parent.TriggerEvent("ue_validaregistro")
			//FileClose(li_valida)
		END IF
			
	END IF
	
LOOP WHILE Message.DoubleParm = 1

//IF dw_2.RowCount() > 0 THEN
//	pb_grabar.Enabled = True
//ELSE
//	pb_grabar.Enabled = False
//END IF


end event

type dw_3 from datawindow within w_captura_archivo_de_cajas
boolean visible = false
integer x = 151
integer y = 656
integer width = 1522
integer height = 536
integer taborder = 40
boolean bringtotop = true
boolean titlebar = true
string title = "palletencab"
string dataobject = "dw_mues_palletencab1"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_captura_archivo_de_cajas
boolean visible = false
integer x = 1719
integer y = 680
integer width = 1522
integer height = 536
integer taborder = 60
boolean bringtotop = true
boolean titlebar = true
string title = "palletfruta"
string dataobject = "dw_mues_palletfruta"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within w_captura_archivo_de_cajas
boolean visible = false
integer x = 123
integer y = 1236
integer width = 1522
integer height = 536
integer taborder = 90
boolean bringtotop = true
boolean titlebar = true
string title = "recfruprocee"
string dataobject = "dw_mues_recfruprocee1"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_6 from datawindow within w_captura_archivo_de_cajas
boolean visible = false
integer x = 1719
integer y = 1264
integer width = 1522
integer height = 536
integer taborder = 90
boolean bringtotop = true
boolean titlebar = true
string title = "recfruproced"
string dataobject = "dw_mues_recfruproced"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

