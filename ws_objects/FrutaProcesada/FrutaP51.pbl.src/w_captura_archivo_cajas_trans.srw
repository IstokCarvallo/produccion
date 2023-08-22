$PBExportHeader$w_captura_archivo_cajas_trans.srw
$PBExportComments$Ventana que captura archivo plano de cajas
forward
global type w_captura_archivo_cajas_trans from w_mant_encab_deta
end type
type pb_archivo from picturebutton within w_captura_archivo_cajas_trans
end type
type dw_3 from datawindow within w_captura_archivo_cajas_trans
end type
type dw_4 from datawindow within w_captura_archivo_cajas_trans
end type
type dw_5 from datawindow within w_captura_archivo_cajas_trans
end type
type dw_6 from datawindow within w_captura_archivo_cajas_trans
end type
type cbx_agrupa from checkbox within w_captura_archivo_cajas_trans
end type
type st_encabe from statictext within w_captura_archivo_cajas_trans
end type
type dw_7 from datawindow within w_captura_archivo_cajas_trans
end type
type dw_agrupa from datawindow within w_captura_archivo_cajas_trans
end type
type st_2 from statictext within w_captura_archivo_cajas_trans
end type
type uo_selplantas from uo_seleccion_plantas within w_captura_archivo_cajas_trans
end type
end forward

global type w_captura_archivo_cajas_trans from w_mant_encab_deta
integer width = 3511
integer height = 2084
string title = "Captura Archivo de Cajas"
string menuname = ""
event ue_carga_cajas ( )
event ue_validaregistro ( )
event ue_carga_spro_cajasprod ( )
pb_archivo pb_archivo
dw_3 dw_3
dw_4 dw_4
dw_5 dw_5
dw_6 dw_6
cbx_agrupa cbx_agrupa
st_encabe st_encabe
dw_7 dw_7
dw_agrupa dw_agrupa
st_2 st_2
uo_selplantas uo_selplantas
end type
global w_captura_archivo_cajas_trans w_captura_archivo_cajas_trans

type variables
String             is_mensaje, is_plantaori

uo_plantadesp      iuo_plantadesp
uo_especie         	iuo_especies
uo_etiquetas       	iuo_etiquetas
uo_variedades     	iuo_variedades	
uo_productores     iuo_productores
uo_prodpredio		iuo_predio
uo_prodcuarteles	iuo_prodcuartel
uo_embalajesprod	iuo_embalajesprod
uo_cliente      		iuo_cliente
uo_calibre      		iuo_calibre
uo_tipopallet		iuo_tipopallet

DatawindowChild    idwc_cliente, idwc_especie, idwc_etiqueta, idwc_variedad, &
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
public function boolean existepallet (integer ai_cliente, integer ai_planta, long al_pallet, boolean ab_mensaje)
public function boolean existepalletendestino (integer ai_cliente, integer ai_planta, long al_pallet)
public function boolean busca_tipopallet ()
end prototypes

event ue_carga_cajas();w_main.SetMicroHelp("Cargando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

Integer	li_planta, li_cliente, li_predio, li_cuartel, li_especie, li_variedad, li_varirotu, li_error, &
         	li_categoria, li_etiqueta, li_seleccio, li_embalado, li_pesadora, li_packespe, li_retorno, li_condicion,&
			li_varrotulada, li_plantaori, li_capr_embala
Long   	ll_guia, ll_pallet, ll_caja, ll_productor, ll_Filas, ll_fila, ll_secuen, ll_FilEncab,&
			ll_find_enca, ll_find_deta, ll_FilFruta, ll_filrecepenca, ll_find_rece, ll_find_tarja,&
			ll_filrecepdeta, ll_numero, ll_docrel, ll_Pesado, ll_Transportista, ll_TipoCamion
Date     	ld_mden_fecdre,ld_fecemb, ld_fecharec, ld_fechaing
String  	ls_mensaje, ls_registro, ls_embalaje, ls_calibre, ls_productrotulado, ls_fecha, ls_Chofer, ls_Patente, ls_Carro
Time		It_horaing


IF dw_1.RowCount() < 0 THEN
	li_Retorno				= MessageBox("Error","Archivo no pudo ser abierto.", Exclamation!, RetryCancel!)
	Message.DoubleParm	= li_Retorno
ELSEIF dw_1.RowCount() = 0 THEN
		MessageBox("Atención", "Archivo no tiene Filas.")
		Message.DoubleParm	=	2
	 ELSE
	   SetPointer(HourGlass!)
END IF
ll_numero = 0
dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()
dw_7.Reset()

FOR ll_Filas = 1 TO dw_1.RowCount()
	li_planta    		= Integer(istr_Mant.Argumento[1])
	ll_guia      		= dw_1.Object.capr_numgia[ll_Filas]
	li_cliente   		= dw_1.Object.clie_codigo[ll_Filas]
	ll_pallet    		= dw_1.Object.capr_numpal[ll_Filas]
	ll_caja      		= dw_1.Object.capr_numero[ll_Filas]
	li_plantaori 		= dw_1.Object.capr_cespak[ll_Filas]  
	ll_Transportista	= dw_1.Object.tran_codigo[ll_Filas]  
	ll_TipoCamion	= dw_1.Object.tica_codigo[ll_Filas]  
	ls_Chofer		= dw_1.Object.defe_chofer[ll_Filas]  
	ls_Patente		= dw_1.Object.defe_patent[ll_Filas]  
	ls_Carro			= dw_1.Object.defe_pataco[ll_Filas]  
	
	IF  ExisteCaja(li_cliente,li_planta,ll_caja, False) THEN
		EXIT
	ELSE		
		ll_productor 	= dw_1.Object.prod_codigo[ll_Filas]
		li_predio    		= dw_1.Object.prod_huerto[ll_Filas]
		li_cuartel   		= dw_1.Object.prod_cuarte[ll_Filas]
		li_especie   		= dw_1.Object.espe_codigo[ll_Filas]
		li_variedad  		= dw_1.Object.capr_varrot[ll_Filas]
		li_varirotu  		= dw_1.Object.plde_codigo[ll_Filas]
		ls_embalaje  	= dw_1.Object.emba_codigo[ll_Filas]
		ld_fecemb    	= dw_1.Object.capr_fecemb[ll_Filas]
		li_categoria 		= dw_1.Object.cate_codigo[ll_Filas]
		li_etiqueta  		= dw_1.Object.etiq_codigo[ll_Filas]
		ls_calibre   		= dw_1.Object.capr_calibr[ll_Filas]
		li_packespe  	= dw_1.Object.capr_cespak[ll_Filas]
		ll_docrel    		= dw_1.Object.capr_docrel[ll_Filas]
		It_horaing	 	= dw_1.Object.capr_hordig[ll_Filas]
		ld_fechaing	 	= dw_1.Object.capr_fecdig[ll_Filas]
		li_capr_embala	= dw_1.Object.capr_embala[ll_Filas]
		ll_Pesado	 	= dw_1.Object.capr_pesado[ll_Filas]
	
		IF IsNull(li_categoria) THEN li_categoria = 1			
		IF Isnull(ll_Pesado) THEN ll_Pesado = 1
				
		/*
		Llena palletencab
		*/
		ll_find_enca	=	dw_3.Find ( "clie_codigo = " + String(li_cliente) + " AND paen_numero = " + String(ll_pallet) + &
								" AND plde_codigo = " + String(li_planta), 1, dw_3.RowCount() )
								
		IF ll_find_enca > 0 THEN
			dw_3.Object.paen_ccajas[ll_find_enca] = dw_3.Object.paen_ccajas[ll_find_enca] + 1	
			IF li_capr_embala = -1 THEN 
				dw_3.Object.tpem_codigo[ll_find_enca] = String(dw_3.Object.paen_ccajas[ll_find_enca])
			END IF
		ELSE	
			ll_FilEncab =	dw_3.InsertRow(0)			
			dw_3.Object.clie_codigo[ll_FilEncab] = li_cliente
			dw_3.Object.plde_codigo[ll_FilEncab] = li_planta
			dw_3.Object.paen_numero[ll_FilEncab] = ll_pallet
			dw_3.Object.espe_codigo[ll_FilEncab] = li_especie
			dw_3.Object.vari_codigo[ll_FilEncab] = li_variedad
			dw_3.Object.cate_codigo[ll_FilEncab] = li_categoria
			dw_3.Object.etiq_codigo[ll_FilEncab] = li_etiqueta
			dw_3.Object.emba_codigo[ll_FilEncab] = ls_embalaje
			dw_3.Object.paen_fecemb[ll_FilEncab] = ld_fecemb
			dw_3.Object.paen_ccajas[ll_FilEncab] = 1	
			dw_3.Object.paen_estado[ll_FilEncab] = 1
			dw_3.Object.prod_codigo[ll_FilEncab] = ll_productor
			dw_3.Object.paen_calibr[ll_FilEncab] = ls_calibre
			dw_3.Object.paen_huert1[ll_FilEncab] = li_predio
			dw_3.Object.paen_cuart1[ll_FilEncab] = li_cuartel
			dw_3.Object.trat_codigo[ll_FilEncab] = 1
			dw_3.Object.paen_inspec[ll_FilEncab] = 0
			dw_3.Object.dest_codigo[ll_FilEncab] = 999
			dw_3.Object.tmvp_codigo[ll_FilEncab] = 1
			dw_3.Object.cama_codigo[ll_FilEncab] = 0
			dw_3.Object.frio_codigo[ll_FilEncab] = '1'	
			
			dw_3.Object.cond_codigo[ll_FilEncab] = 0
			dw_3.Object.paen_calle[ll_FilEncab]  = 1
			dw_3.Object.paen_base[ll_FilEncab]   = 1
			dw_3.Object.paen_posici[ll_FilEncab] = 1
			dw_3.Object.stat_codigo[ll_FilEncab] = 1
			
			dw_3.Object.esta_codigo[ll_FilEncab]   = 2
			dw_3.Object.paen_fecpro[ll_FilEncab] = Datetime(Today(), Now())
//			dw_3.Object.paen_fecter[ll_FilEncab] = 
			
			dw_3.Object.paen_tipopa[ll_FilEncab] = Abs(li_capr_embala)
			IF li_capr_embala = -1 THEN 
				dw_3.Object.tpem_codigo[ll_FilEncab] = "1"
			END IF
			dw_3.Object.paen_cosecha[ll_FilEncab] = ld_fecemb
			dw_3.Object.copa_codigo[ll_FilEncab] = ll_Pesado
		END IF
		
		/*
		Llena palletfruta 
		*/
		li_condicion = 0		

		li_varrotulada 	= buscavarrelacionada(li_especie,li_variedad)	
		ls_productrotulado= buscaprodrotulado(li_cliente,ll_productor)

		ll_FilFruta =	dw_4.InsertRow(0)			
		dw_4.Object.clie_codigo[ll_FilFruta] = li_cliente
		dw_4.Object.plde_codigo[ll_FilFruta] = li_planta
		dw_4.Object.paen_numero[ll_FilFruta] = ll_pallet
		dw_4.Object.espe_codigo[ll_FilFruta] = li_especie
		dw_4.Object.vari_codigo[ll_FilFruta] = li_variedad
		dw_4.Object.cond_codigo[ll_FilFruta] = li_condicion
		dw_4.Object.etiq_codigo[ll_FilFruta] = li_etiqueta
		dw_4.Object.emba_codigo[ll_FilFruta] = ls_embalaje
		dw_4.Object.pafr_fecemb[ll_FilFruta] = ld_fecemb
		dw_4.Object.pafr_ccajas[ll_FilFruta] = 1	
		dw_4.Object.pafr_secuen[ll_FilFruta] = ll_caja 			
		dw_4.Object.prod_codigo[ll_FilFruta] = ll_productor
		dw_4.Object.pafr_calibr[ll_FilFruta] = ls_calibre
		dw_4.Object.pafr_huert1[ll_FilFruta] = li_predio
		dw_4.Object.pafr_cuart1[ll_FilFruta] = li_cuartel
		dw_4.Object.pafr_copack[ll_FilFruta] = li_packespe
		dw_4.Object.pafr_varrot[ll_FilFruta] = li_varrotulada
		dw_4.Object.pafr_prdrot[ll_FilFruta] = Long(ls_productrotulado)
		dw_4.Object.pafr_fecing[ll_FilFruta] = Date(Today())
		
		/*
		carga recepcion encabezado
		*/
		ld_fecharec = date(today())
		ls_fecha = String(ld_fecharec)
		
		IF ll_Filas = 1 THEN
			//ll_find_rece	=	dw_5.Find ( "plde_codigo = " + String(li_planta)+ &
			//						" AND String(rfpe_fecrec, 'dd/mm/yyyy') = '" + String(Date(ls_fecha), 'dd/mm/yyyy')+"'", 1, dw_5.RowCount() )
			//IF ll_find_rece = 0 THEN
				////ll_numero = buscanumero(li_planta)
				//ll_numero = buscanuevofolio(li_cliente,li_planta) 
				ll_numero	= ll_caja
				
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
				
				dw_5.Object.tran_codigo[ll_filrecepenca] = ll_Transportista
				dw_5.Object.tica_codigo[ll_filrecepenca] = ll_TipoCamion
				dw_5.Object.rfpe_chofer[ll_filrecepenca] = ls_Chofer
				dw_5.Object.rfpe_patent[ll_filrecepenca] = ls_Patente
				dw_5.Object.rfpe_patcar[ll_filrecepenca] = ls_Carro
			//END IF
		END IF
			
		/*  
		carga recepcion detalle
		*/
		ll_find_tarja	=	dw_6.Find ( "clie_codigo = " + String(li_cliente) + " AND paen_numero = " + String(ll_pallet) + &
						" AND rfpe_numero = " + String(ll_numero) + " AND plde_codigo = " + String(li_planta), 1, dw_6.RowCount() )
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

event ue_validaregistro();Integer	li_cliente, li_planta, li_predio
String	ls_mensaje, ls_colu[], ls_calibre, ls_calibre1,ls_Embalaje,ls_tpem_codigo, &
			ls_capr_embala, ls_Nulo
Long     ll_fila, ll_caja, li_cont

If dw_1.RowCount() >0 Then	
   FOR ll_fila = 1 to dw_1.RowCount()
		li_cliente   	= dw_1.Object.clie_codigo[ll_fila]
		li_planta   	= uo_SelPlantas.Codigo
		ll_caja  	   	= dw_1.Object.capr_numero[ll_fila]
		
		li_predio    	= dw_1.Object.prod_huerto[ll_fila]
		
		If  ExisteCaja(li_cliente,li_planta,ll_caja, False) Then
			li_cont ++
			ls_mensaje 		 = ls_mensaje + "~nCaja "+ String(dw_1.Object.capr_numero[ll_fila]) + &
									" Fila : " + String(ll_fila) +" Existe"
			ls_colu[li_cont]= "capr_numero"			
		End If
		
		If NOT iuo_cliente.existe(dw_1.Object.clie_codigo[ll_fila],False,sqlca) Then
			li_cont ++
			ls_mensaje 		 = ls_mensaje + "~nCliente "+ String(dw_1.Object.clie_codigo[ll_fila]) + &
									" Fila : " + String(ll_fila) 
			ls_colu[li_cont]= "clie_codigo"
		End If
		
		If NOT iuo_plantadesp.existe(dw_1.Object.plde_codigo[ll_fila],False,sqlca) Then
			li_cont ++
			ls_mensaje 		 = ls_mensaje + "~nPacking "+ String(dw_1.Object.plde_codigo[ll_fila]) + &
									" Fila : " + String(ll_fila) 
			ls_colu[li_cont]= "plde_codigo"
		End If
		
		If NOT Isnull(dw_1.Object.prod_codigo[ll_fila]) AND dw_1.Object.prod_codigo[ll_fila] > 0 Then
			If NOT iuo_productores.existe(dw_1.Object.prod_codigo[ll_fila],False,sqlca) Then
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nProductor "+ String(dw_1.Object.prod_codigo[ll_fila]) + &
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "prod_codigo"
			End If 
		End If
		
		If NOT Isnull(dw_1.Object.prod_predio[ll_fila]) AND dw_1.Object.prod_predio[ll_fila] > 0 Then
			If NOT iuo_predio.existepredioprod(dw_1.Object.prod_codigo[ll_fila],dw_1.Object.prod_predio[ll_fila],False,sqlca) Then
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nPredio "+ String(dw_1.Object.prod_predio[ll_fila]) + &
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "prod_predio"
			End If
		End If
		If NOT Isnull(dw_1.Object.prod_cuarte[ll_fila]) AND dw_1.Object.prod_cuarte[ll_fila] > 0 Then
			If NOT iuo_prodcuartel.existe(dw_1.Object.prod_codigo[ll_fila],dw_1.Object.prod_predio[ll_fila],dw_1.Object.prod_cuarte[ll_fila],False,sqlca) Then
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nCuartel " + String(dw_1.Object.prod_cuarte[ll_fila]) + &
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "prod_cuarte"	
			End If
		End If
		
		If NOT Isnull(dw_1.Object.espe_codigo[ll_fila]) AND dw_1.Object.espe_codigo[ll_fila] > 0 Then
			If NOT iuo_especies.existe(dw_1.Object.espe_codigo[ll_fila],False,sqlca) Then
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nEspecie " + String(dw_1.Object.espe_codigo[ll_fila]) + &
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "espe_codigo"	  
			End If
		End If
		
		If NOT Isnull(dw_1.Object.vari_codigo[ll_fila]) AND dw_1.Object.vari_codigo[ll_fila] > 0 Then
			If NOT iuo_variedades.existe(dw_1.Object.espe_codigo[ll_fila],dw_1.Object.vari_codigo[ll_fila],False,sqlca) Then
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nVariedad " + String(dw_1.Object.vari_codigo[ll_fila]) + &
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "vari_codigo"	   
			End If
		End If
		
		If NOT Isnull(dw_1.Object.capr_varrot[ll_fila]) AND dw_1.Object.capr_varrot[ll_fila] > 0 Then
			If NOT iuo_variedades.existe(dw_1.Object.espe_codigo[ll_fila],dw_1.Object.capr_varrot[ll_fila],False,sqlca) Then
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nVariedad Rotulada " + String(dw_1.Object.capr_varrot[ll_fila]) + &
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "capr_varrot"	   
			End If
		End If
		
		If NOT Isnull(dw_1.Object.emba_codigo[ll_fila]) AND dw_1.Object.emba_codigo[ll_fila] <>"" Then
			If NOT iuo_embalajesprod.existe(dw_1.Object.clie_codigo[ll_fila],dw_1.Object.emba_codigo[ll_fila],False,sqlca) Then
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nEmbalaje " + dw_1.Object.emba_codigo[ll_fila] + &
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "emba_codigo"	     
			End If
		End If
		
		If NOT Isnull(dw_1.Object.etiq_codigo[ll_fila]) AND dw_1.Object.etiq_codigo[ll_fila] > 0  Then
			If NOT iuo_etiquetas.existe(dw_1.Object.etiq_codigo[ll_fila],False,sqlca) Then
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nEtiqueta " + String(dw_1.Object.etiq_codigo[ll_fila]) + &
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "etiq_codigo"	       
			End If
		End If	
		
		If NOT Isnull(dw_1.Object.capr_calibr[ll_fila]) AND dw_1.Object.capr_calibr[ll_fila] <> "" Then
			ls_calibre1 = Trim(dw_1.Object.capr_calibr[ll_fila])
			ls_calibre = ls_calibre1//+''+'   '
			//ls_calibre = left(ls_calibre,3)
			If NOT iuo_calibre.existe(dw_1.Object.espe_codigo[ll_fila],dw_1.Object.vari_codigo[ll_fila],ls_calibre,False,sqlca) Then
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nCalibre " + ls_calibre + & 
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "capr_calibr"	
				Return
			End If
		End If
		
		If NOT Isnull(dw_1.Object.capr_calrot[ll_fila]) AND dw_1.Object.capr_calrot[ll_fila] <> "" Then
			ls_calibre1 = Trim(dw_1.Object.capr_calrot[ll_fila])
			ls_calibre = ls_calibre1
			If NOT iuo_calibre.existe(dw_1.Object.espe_codigo[ll_fila],dw_1.Object.vari_codigo[ll_fila],ls_calibre,False,sqlca) Then
				li_cont ++
				ls_mensaje 		 = ls_mensaje + "~nCalibre " + ls_calibre + & 
										" Fila : " + String(ll_fila) 
				ls_colu[li_cont]= "capr_calrot"	   
				Return
			End If
		End If
	NEXT
End If

If li_cont > 0 Then
	MessageBox("Error de Consistencia", "Código de " + ls_mensaje + " No Existe en tabla respectiva.", StopSign!, Ok!)
	dw_1.Reset()
	Message.DoubleParm = -1
	pb_archivo.SetFocus()
End If
end event

event ue_carga_spro_cajasprod();//w_main.SetMicroHelp("Cargando Datos...")
//SetPointer(HourGlass!)
//PostEvent("ue_listo")
//
//Integer     li_planta, li_cliente, li_predio, li_cuartel, li_especie, li_variedad, li_varirotu, li_error, &
//            li_categoria, li_etiqueta, li_seleccio, li_embalado, li_pesadora, li_packespe, li_retorno, li_condicion,&
//				li_varrotulada, li_plantaori, li_capr_embala, li_busca, li_ClienteAnt, li_abulta, li_tipopa, li_packrot
//Long        ll_guia, ll_pallet, ll_caja, ll_productor, ll_Filas, ll_fila, ll_secuen, ll_FilEncab,&
//				ll_find_enca, ll_find_deta, ll_FilFruta, ll_filrecepenca, ll_find_rece, ll_find_tarja,&
//				ll_filrecepdeta, ll_numero, ll_docrel, ll_Pesado, ll_cajas, ll_PalletAnt, ll_SproCajas, ll_ccajas
//Date        ld_mden_fecdre,ld_fecemb, ld_fecharec, ld_fechaing
//String      ls_mensaje, ls_registro, ls_embalaje, ls_calibre, ls_productrotulado, ls_fecha,ls_tpem_codigo
//Time			It_horaing
//
//IF dw_1.RowCount() < 0 THEN
//	li_Retorno				= MessageBox("Error","Archivo no pudo ser abierto.", Exclamation!, &
//												RetryCancel!)
//	Message.DoubleParm	= li_Retorno
//ELSEIF dw_1.RowCount() = 0 THEN
//		MessageBox("Atención", "Archivo no tiene Filas.")
//		Message.DoubleParm	=	2
//	 ELSE
//	   SetPointer(HourGlass!)
//END IF
//
//IF cbx_agrupa.Checked THEN
//	li_abulta	=	1
//ELSE
//	li_abulta	=	0
//END IF
//
//ll_numero 		= 	0
//
//ll_Secuen		= 	0
//li_ClienteAnt	= 	0
//ll_PalletAnt	= 	0
//
//dw_3.Reset()
//dw_4.Reset()
//dw_5.Reset()
//dw_6.Reset()
//dw_7.Reset()
//
//li_planta    = Integer(istr_Mant.Argumento[1])
//
//FOR ll_Filas = 1 TO dw_1.RowCount()
//	
//	 li_cliente   = dw_1.Object.clie_codigo[ll_Filas]
//	 ll_pallet    = dw_1.Object.capr_numpal[ll_Filas]
//	
//	 IF ExistePalletEnDestino(li_cliente,li_planta,ll_pallet) THEN
//	
////			ll_SproCajas =	dw_7.InsertRow(0)
////			dw_7.Object.clie_codigo[ll_SproCajas] 	= 	dw_1.Object.clie_codigo[ll_Filas]
////			dw_7.Object.plde_codigo[ll_SproCajas] 	= 	li_planta
////			dw_7.Object.capr_numgia[ll_SproCajas]	=	dw_1.Object.capr_numgia[ll_Filas]
////			dw_7.Object.capr_numero[ll_SproCajas]	=	dw_1.Object.capr_numero[ll_Filas]
////			dw_7.Object.capr_numpal[ll_SproCajas]	=	dw_1.Object.capr_numpal[ll_Filas]
////			dw_7.Object.capr_cespak[ll_SproCajas]	=	dw_1.Object.capr_cespak[ll_Filas]
////			dw_7.Object.prod_codigo[ll_SproCajas]	=	dw_1.Object.prod_codigo[ll_Filas]
////			dw_7.Object.prod_huerto[ll_SproCajas]	=	dw_1.Object.prod_huerto[ll_Filas]
////			dw_7.Object.prod_cuarte[ll_SproCajas]	=	dw_1.Object.prod_cuarte[ll_Filas]
////			dw_7.Object.espe_codigo[ll_SproCajas]	=	dw_1.Object.espe_codigo[ll_Filas]
////			dw_7.Object.capr_varrot[ll_SproCajas]	=	dw_1.Object.capr_varrot[ll_Filas]
////			dw_7.Object.emba_codigo[ll_SproCajas]	=	dw_1.Object.emba_codigo[ll_Filas]
////			dw_7.Object.capr_fecemb[ll_SproCajas]	=	dw_1.Object.capr_fecemb[ll_Filas]
////			dw_7.Object.cate_codigo[ll_SproCajas]	=	dw_1.Object.cate_codigo[ll_Filas]
////			dw_7.Object.etiq_codigo[ll_SproCajas]	=	dw_1.Object.etiq_codigo[ll_Filas]
////			dw_7.Object.capr_calibr[ll_SproCajas]	=	dw_1.Object.capr_calibr[ll_Filas]
////			dw_7.Object.capr_cespak[ll_SproCajas]	=	dw_1.Object.capr_cespak[ll_Filas]
////			dw_7.Object.capr_docrel[ll_SproCajas]	=	dw_1.Object.capr_docrel[ll_Filas]
////			dw_7.Object.capr_hordig[ll_SproCajas]	=	dw_1.Object.capr_hordig[ll_Filas]
////			dw_7.Object.capr_fecdig[ll_SproCajas]	=	dw_1.Object.capr_fecdig[ll_Filas]
////			dw_7.Object.capr_embala[ll_SproCajas]	=	dw_1.Object.capr_embala[ll_Filas]
////			dw_7.Object.capr_pesado[ll_SproCajas]	=	dw_1.Object.capr_pesado[ll_Filas]
//			
//		
//			ll_guia      = dw_1.Object.capr_numgia[ll_Filas]
//			ll_caja      = dw_1.Object.capr_numero[ll_Filas]
//			li_plantaori = dw_1.Object.capr_cespak[ll_Filas]  
//			
//			IF  ExisteCaja(li_cliente,li_planta,ll_caja, False) THEN
//				EXIT
//			ELSE		
//				ll_productor 	= dw_1.Object.prod_codigo[ll_Filas]
//				li_predio    	= dw_1.Object.prod_huerto[ll_Filas]
//				li_cuartel   	= dw_1.Object.prod_cuarte[ll_Filas]
//				li_especie   	= dw_1.Object.espe_codigo[ll_Filas]
//				li_variedad  	= dw_1.Object.vari_codigo[ll_Filas]
//				li_varirotu  	= dw_1.Object.capr_varrot[ll_Filas]
//				ls_embalaje  	= dw_1.Object.emba_codigo[ll_Filas]
//				ld_fecemb    	= dw_1.Object.capr_fecemb[ll_Filas]
//				li_categoria 	= dw_1.Object.cate_codigo[ll_Filas]
//				li_etiqueta  	= dw_1.Object.etiq_codigo[ll_Filas]
//				ls_calibre   	= dw_1.Object.capr_calibr[ll_Filas]
//				li_packespe  	= dw_1.Object.capr_cespak[ll_Filas]
//				ll_docrel    	= dw_1.Object.capr_docrel[ll_Filas]
//				It_horaing	 	= dw_1.Object.capr_hordig[ll_Filas]
//				ld_fechaing	 	= dw_1.Object.capr_fecdig[ll_Filas]
//				li_capr_embala	= dw_1.Object.capr_embala[ll_Filas]
//				ll_Pesado	 	= dw_1.Object.capr_pesado[ll_Filas]
//				li_tipopa		= dw_1.Object.paen_tipopa[ll_Filas]
//				li_packrot		= dw_1.Object.pafr_rotpak[ll_Filas]
//				ll_ccajas 		= dw_1.Object.pafr_ccajas[ll_Filas]
//				
//				IF IsNull(li_categoria) THEN li_categoria = 1			
//				IF Isnull(ll_Pesado) THEN ll_Pesado = 1
//						
//				/*
//				Llena palletencab
//				*/
//				ll_find_enca	=	dw_3.Find ( "clie_codigo = " + String(li_cliente) + &
//										" AND paen_numero = " + String(ll_pallet) + &
//										" AND plde_codigo = " + String(li_planta), 1, dw_3.RowCount() )
//										
//				IF ll_find_enca > 0 THEN
//					dw_3.Object.paen_ccajas[ll_find_enca] = dw_3.Object.paen_ccajas[ll_find_enca] + 1	
//					IF li_capr_embala = 1 THEN 
//						dw_3.Object.tpem_codigo[ll_find_enca] = String(dw_3.Object.paen_ccajas[ll_find_enca])
//						ls_tpem_codigo	= dw_3.Object.tpem_codigo[ll_find_enca]			
//						IF iuo_tipopallet.existe(li_cliente,ls_embalaje,ls_tpem_codigo,False,sqlca) THEN
//							dw_3.Object.paen_altura[ll_find_enca] = iuo_tipopallet.Altura
//						END IF
//					END IF
//				ELSE	
//					ll_FilEncab =	dw_3.InsertRow(0)			
//					dw_3.Object.clie_codigo[ll_FilEncab] = li_cliente
//					dw_3.Object.plde_codigo[ll_FilEncab] = li_planta
//					dw_3.Object.paen_numero[ll_FilEncab] = ll_pallet
//					dw_3.Object.espe_codigo[ll_FilEncab] = li_especie
//					dw_3.Object.vari_codigo[ll_FilEncab] = li_variedad
//					dw_3.Object.cate_codigo[ll_FilEncab] = li_categoria
//					dw_3.Object.etiq_codigo[ll_FilEncab] = li_etiqueta
//					dw_3.Object.emba_codigo[ll_FilEncab] = ls_embalaje
//					dw_3.Object.paen_fecemb[ll_FilEncab] = ld_fecemb
//					dw_3.Object.paen_fecini[ll_FilEncab] = ld_fecemb
//					dw_3.Object.paen_ccajas[ll_FilEncab] = 1	
//					dw_3.Object.paen_estado[ll_FilEncab] = 1
//					dw_3.Object.prod_codigo[ll_FilEncab] = ll_productor
//					dw_3.Object.paen_calibr[ll_FilEncab] = ls_calibre
//					dw_3.Object.paen_huert1[ll_FilEncab] = li_predio
//					dw_3.Object.paen_cuart1[ll_FilEncab] = li_cuartel
//					dw_3.Object.trat_codigo[ll_FilEncab] = 1
//					dw_3.Object.paen_inspec[ll_FilEncab] = 0
//					dw_3.Object.dest_codigo[ll_FilEncab] = 999
//					dw_3.Object.tmvp_codigo[ll_FilEncab] = 1
//					dw_3.Object.cama_codigo[ll_FilEncab] = 0
//					dw_3.Object.frio_codigo[ll_FilEncab] = '1'	
//					
//					dw_3.Object.cond_codigo[ll_FilEncab] = 0
//					dw_3.Object.paen_calle[ll_FilEncab]  = 1
//					dw_3.Object.paen_base[ll_FilEncab]   = 1
//					dw_3.Object.paen_posici[ll_FilEncab] = 1
//					dw_3.Object.stat_codigo[ll_FilEncab] = 1
//					dw_3.Object.paen_concal[ll_FilEncab] = 1
//					//dw_3.Object.paen_pcopda[ll_FilEncab] = 2
//					dw_3.Object.paen_tipopa[ll_FilEncab] = li_tipopa
//					
//					dw_3.Object.paen_tipopa[ll_FilEncab] = li_capr_embala
//					IF li_capr_embala = 1 THEN 
//						dw_3.Object.tpem_codigo[ll_FilEncab] = "1"
//						ls_tpem_codigo	= dw_3.Object.tpem_codigo[ll_FilEncab]			
//						IF iuo_tipopallet.existe(li_cliente,ls_embalaje,ls_tpem_codigo,False,sqlca) THEN
//							dw_3.Object.paen_altura[ll_FilEncab] = iuo_tipopallet.Altura
//						END IF
//					END IF
//					dw_3.Object.paen_cosecha[ll_FilEncab] = ld_fecemb
//					dw_3.Object.copa_codigo[ll_FilEncab] = ll_Pesado
//				END IF
//				
//				/*
//				Llena palletfruta 
//				*/
//				IF li_abulta	=	1  THEN  //AGRUPA POR CARACERISTICAS
//						IF li_ClienteAnt	<> li_cliente OR ll_PalletAnt <> ll_pallet THEN
//							ll_Secuen 		= 0		
//							li_ClienteAnt	= li_cliente
//							ll_PalletAnt	= ll_pallet			
//						END IF		
//				
//						li_condicion = 0		
//						
//						li_busca = dw_4.Find("clie_codigo   = "+String(li_cliente) + &
//													" AND paen_numero= "+String(ll_pallet) + &
//													" AND espe_codigo= "+String(li_especie) + &
//													" AND vari_codigo= "+String(li_variedad) +&
//													" AND emba_codigo= '"+ls_embalaje + "'"	 +&
//													" AND prod_codigo= "+String(ll_productor)+ &
//													" AND cond_codigo= "+String(li_condicion) + &
//													" AND etiq_codigo= "+String(li_etiqueta) + &
//													" AND plde_codigo= "+String(li_planta) + &
//													" AND pafr_calibr = '"+ ls_calibre+ "'",1,dw_4.RowCount())							    					  
//					
//						IF li_busca = 0 THEN
//							li_busca	=	dw_4.InsertRow(0)
//							dw_4.SetItem(li_busca,'clie_codigo',li_cliente)
//							dw_4.SetItem(li_busca,'paen_numero',ll_pallet)
//							dw_4.SetItem(li_busca,'espe_codigo',li_especie)
//							dw_4.SetItem(li_busca,'vari_codigo',li_variedad)
//							dw_4.SetItem(li_busca,'emba_codigo',ls_embalaje)
//							dw_4.SetItem(li_busca,'prod_codigo',ll_productor)
//							dw_4.SetItem(li_busca,'cond_codigo',li_condicion)
//							dw_4.SetItem(li_busca,'etiq_codigo',li_etiqueta)
//							dw_4.SetItem(li_busca,'plde_codigo',li_planta)		
//							dw_4.SetItem(li_busca,'pafr_calibr',ls_calibre)
//							ll_Secuen++
//							dw_4.SetItem(li_busca,'pafr_secuen',ll_Secuen)
//							dw_4.SetItem(li_busca,'pafr_rotpak',li_packrot)
//						END IF		
//						li_varrotulada 	= buscavarrelacionada(li_especie,li_variedad)	
//						ls_productrotulado= buscaprodrotulado(li_cliente,ll_productor)
//						
//						dw_4.Object.pafr_fecemb[li_busca] = ld_fecemb
//						dw_4.Object.pafr_huert1[li_busca] = li_predio
//						dw_4.Object.pafr_cuart1[li_busca] = li_cuartel
//						dw_4.Object.pafr_copack[li_busca] = li_packespe
//						dw_4.Object.pafr_varrot[li_busca] = li_varrotulada
//						dw_4.Object.pafr_prdrot[li_busca] = ll_productor //Long(ls_productrotulado)
//						dw_4.Object.pafr_fecing[li_busca] = Date(Today())
//						dw_4.Object.pafr_rotpak[li_busca] = li_packrot
//						
//						ll_cajas	= dw_4.GetItemNumber(li_busca,'pafr_ccajas')
//						
//						dw_4.Object.pafr_ccajas[li_busca] = ll_ccajas	
//				ELSE //ABIERTO CAJA A CAJA, SECUENCIA REPRESENTA NUMERO DE CAJA
//						li_busca	=	dw_4.InsertRow(0)
//						dw_4.SetItem(li_busca,'clie_codigo',li_cliente)
//						dw_4.SetItem(li_busca,'paen_numero',ll_pallet)
//						dw_4.SetItem(li_busca,'espe_codigo',li_especie)
//						dw_4.SetItem(li_busca,'vari_codigo',li_variedad)
//						dw_4.SetItem(li_busca,'emba_codigo',ls_embalaje)
//						dw_4.SetItem(li_busca,'prod_codigo',ll_productor)
//						dw_4.SetItem(li_busca,'cond_codigo',li_condicion)
//						dw_4.SetItem(li_busca,'etiq_codigo',li_etiqueta)
//						dw_4.SetItem(li_busca,'plde_codigo',li_planta)		
//						dw_4.SetItem(li_busca,'pafr_calibr',ls_calibre)
//						dw_4.SetItem(li_busca,'pafr_secuen',ll_caja)
//						li_varrotulada 	= buscavarrelacionada(li_especie,li_variedad)	
//						ls_productrotulado= buscaprodrotulado(li_cliente,ll_productor)
//						dw_4.Object.pafr_fecemb[li_busca] = ld_fecemb
//						dw_4.Object.pafr_huert1[li_busca] = li_predio
//						dw_4.Object.pafr_cuart1[li_busca] = li_cuartel
//						dw_4.Object.pafr_copack[li_busca] = li_packespe
//						dw_4.Object.pafr_varrot[li_busca] = li_varrotulada
//						dw_4.Object.pafr_prdrot[li_busca] = ll_productor
//						dw_4.Object.pafr_fecing[li_busca] = Date(Today())
//						dw_4.Object.pafr_ccajas[li_busca] = ll_ccajas		
//						dw_4.Object.pafr_rotpak[li_busca] = li_packrot
//		
//				END IF
//				
//				/*
//				carga recepcion encabezado
//				*/
//				ld_fecharec = date(today()) 
//				ls_fecha = String(ld_fecharec)
//				
//				/*  
//				carga recepcion encabezado
//				*/
//				ll_numero	= dw_3.Object.paen_numero[1]
//				
//				ll_find_tarja	=	dw_5.Find ( "clie_codigo = " + String(li_cliente) + &
//								" AND rfpe_numero = " + String(ll_numero) + &
//								" AND plde_codigo = " + String(li_planta), 1, dw_5.RowCount() )				
//				
//				IF ll_find_tarja = 0 THEN
//						ll_filrecepenca =	dw_5.InsertRow(0)				
//						dw_5.Object.clie_codigo[ll_filrecepenca] = li_cliente
//						dw_5.Object.plde_codigo[ll_filrecepenca] = li_planta
//						dw_5.Object.rfpe_numero[ll_filrecepenca] = ll_numero
//						dw_5.Object.rfpe_ptaori[ll_filrecepenca] = li_packespe
//						dw_5.Object.rfpe_fecrec[ll_filrecepenca] = Date(Today())
//						dw_5.Object.rfpe_guides[ll_filrecepenca] = ll_guia
//						dw_5.Object.tpmv_codigo[ll_filrecepenca] = 1
//						dw_5.Object.rfpe_tipoen[ll_filrecepenca] = 1
//						dw_5.Object.prod_codigo[ll_filrecepenca] = ll_productor
//						dw_5.Object.rfpe_horrec[ll_filrecepenca] = Now()
//						dw_5.Object.rfpe_fecing[ll_filrecepenca] = ld_fechaing
//						dw_5.Object.rfpe_horing[ll_filrecepenca] = It_horaing
//						dw_5.Object.rfpe_pcopda[ll_filrecepenca] = 2
//				END IF
//					
//				/*  
//				carga recepcion detalle
//				*/
//				ll_find_tarja	=	dw_6.Find ( "clie_codigo = " + String(li_cliente) + &
//								" AND paen_numero = " + String(ll_pallet) + &
//								" AND rfpe_numero = " + String(ll_numero) + &
//								" AND plde_codigo = " + String(li_planta), 1, dw_6.RowCount() )
//				
//				IF ll_find_tarja = 0 THEN
//					ll_filrecepdeta = dw_6.InsertRow(0)			
//					dw_6.Object.clie_codigo[ll_filrecepdeta] = li_cliente
//					dw_6.Object.plde_codigo[ll_filrecepdeta] = li_planta
//					dw_6.Object.paen_numero[ll_filrecepdeta] = ll_pallet
//					dw_6.Object.rfpe_numero[ll_filrecepdeta] = ll_numero
//					dw_6.Object.rfpe_pcopda[ll_filrecepdeta] = 2
//				END IF
//			END IF		
//		
//	ELSE
//			//MessageBox("Error", "Pallet Nro." + String(ll_Pallet) + " Ya Existe en Destino, No se Cargará")
//
//	END IF
//	
//	
//NEXT
//
//SetPointer(Arrow!)
//dw_1.SetRedraw(True)
//
//Message.DoubleParm 	= li_retorno

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
    FROM dbo.variedades  
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
    FROM dbo.productoresprod  
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

Borrando	=	False

IF Borrando THEN
//	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			IF dw_3.Update(True, False) = 1 THEN
				IF dw_4.Update(True, False) = 1 THEN
					IF dw_5.Update(True, False) = 1 THEN
						IF dw_6.Update(True, False) = 1 THEN
							IF dw_7.Update(True, False) = 1 THEN
					Commit;
					
							IF sqlca.SQLCode <> 0 THEN
								F_ErrorBaseDatos(sqlca, This.Title)
								
								RollBack;
							ELSE
								lb_Retorno	=	True
								
//								dw_1.ResetUpdate()
								dw_2.ResetUpdate()
								dw_3.ResetUpdate()
								dw_4.ResetUpdate()
								dw_5.ResetUpdate()
								dw_6.ResetUpdate()
								dw_7.ResetUpdate()
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
//	ELSE
//		F_ErrorBaseDatos(sqlca, This.Title)
//		
//		RollBack;
//	END IF		
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			IF dw_3.Update(True, False) = 1 THEN
				IF dw_4.Update(True, False) = 1 THEN
					IF dw_5.Update(True, False) = 1 THEN
						IF dw_6.Update(True, False) = 1 THEN
							IF dw_7.Update(True, False) = 1 THEN
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
								dw_7.ResetUpdate()
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
    FROM dbo.recfruprocee  
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
  FROM dbo.RECFRUPROCEE
 WHERE plde_codigo = :li_planta;
 
Select como_inicia, como_actual, como_termin
Into	:ll_numero2, :ll_actual, :ll_fin
from dbo.CORRELMOVIMIENTOS 
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
  FROM dbo.spro_cajasprod_trans 
  WHERE clie_codigo = :ai_cliente AND  
        plde_codigo = :ai_planta  AND  
        capr_numero = :al_caja;

IF sqlca.SQLCode =-1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Spro_cajasprod_trans")
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

public function boolean existepallet (integer ai_cliente, integer ai_planta, long al_pallet, boolean ab_mensaje);Long ll_numero

SELECT paen_numero  
  INTO :ll_numero  
  FROM dbo.Palletencab_trans 
  WHERE clie_codigo = :ai_cliente AND  
        plde_codigo = :ai_planta  AND  
        paen_numero = :al_pallet;

IF sqlca.SQLCode =-1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Spro_cajasprod_trans")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	RETURN FALSE
ELSEIf ab_mensaje AND IsNull(ll_numero) AND ll_numero=0 THEN
	MessageBox("Atención","Archivo de Cajas ya fue Ingresado.",&
							Exclamation!, OK!)
	RETURN TRUE
ELSE
	RETURN TRUE
END IF




end function

public function boolean existepalletendestino (integer ai_cliente, integer ai_planta, long al_pallet);Long ll_numero

SELECT paen_numero  
  INTO :ll_numero  
  FROM dbo.Palletencab
  WHERE clie_codigo = :ai_cliente AND  
        plde_codigo = :ai_planta  AND  
        paen_numero = :al_pallet;

IF sqlca.SQLCode =-1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Palletencab")
	RETURN False
ELSEIF sqlca.SQLCode = 100 OR IsNull(ll_numero) OR ll_numero=0 THEN

		SELECT paen_numero  
	   INTO :ll_numero  
  		FROM dbo.Palletencab_trans
  		WHERE clie_codigo = :ai_cliente AND  
        		plde_codigo = :ai_planta  AND  
        		paen_numero = :al_pallet;
	
		IF sqlca.SQLCode =-1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Palletencab_Trans")
			RETURN False
		ELSEIF sqlca.SQLCode = 100 OR IsNull(ll_numero) OR ll_numero=0 THEN

			RETURN TRUE
		ELSE
			RETURN False
		END IF
ELSE
	RETURN False
END IF


end function

public function boolean busca_tipopallet ();Long ll_numero
Integer	li_fila, li_cont, li_cliente
String	ls_cajas, ls_embalaje

FOR li_fila = 1 TO dw_agrupa.RowCount()
NEXT

Return False

end function

on w_captura_archivo_cajas_trans.create
int iCurrent
call super::create
this.pb_archivo=create pb_archivo
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_5=create dw_5
this.dw_6=create dw_6
this.cbx_agrupa=create cbx_agrupa
this.st_encabe=create st_encabe
this.dw_7=create dw_7
this.dw_agrupa=create dw_agrupa
this.st_2=create st_2
this.uo_selplantas=create uo_selplantas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_archivo
this.Control[iCurrent+2]=this.dw_3
this.Control[iCurrent+3]=this.dw_4
this.Control[iCurrent+4]=this.dw_5
this.Control[iCurrent+5]=this.dw_6
this.Control[iCurrent+6]=this.cbx_agrupa
this.Control[iCurrent+7]=this.st_encabe
this.Control[iCurrent+8]=this.dw_7
this.Control[iCurrent+9]=this.dw_agrupa
this.Control[iCurrent+10]=this.st_2
this.Control[iCurrent+11]=this.uo_selplantas
end on

on w_captura_archivo_cajas_trans.destroy
call super::destroy
destroy(this.pb_archivo)
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.dw_6)
destroy(this.cbx_agrupa)
destroy(this.st_encabe)
destroy(this.dw_7)
destroy(this.dw_agrupa)
destroy(this.st_2)
destroy(this.uo_selplantas)
end on

event open;call super::open;Boolean lb_Cerrar 

If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelPlantas.Seleccion(False, False)
	uo_SelPlantas.Inicia(gi_CodPlanta) 
	
	dw_3.SetTransObject(sqlca)
	dw_4.SetTransObject(sqlca)
	dw_5.SetTransObject(sqlca)
	dw_6.SetTransObject(sqlca)
	dw_7.SetTransObject(sqlca)
	dw_agrupa.SetTransObject(sqlca)
	
	pb_archivo.PostEvent(Clicked!)
	
	iuo_plantadesp     		= CREATE  uo_plantadesp      
	iuo_especies       		= CREATE  uo_especie 
	iuo_etiquetas      		= CREATE  uo_etiquetas       
	iuo_variedades	    		= CREATE  uo_variedades
	iuo_productores    	= CREATE  uo_productores
	iuo_predio         		= CREATE  uo_prodpredio
	iuo_prodcuartel    		= CREATE  uo_prodcuarteles
	iuo_embalajesprod 	= CREATE  uo_embalajesprod
	iuo_cliente        		= CREATE  uo_cliente
	iuo_calibre        		= CREATE  uo_calibre
	iuo_tipopallet		 	= CREATE  uo_tipopallet		
	
End If
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
dw_7.Reset()
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.SetFocus()

pb_archivo.Enabled = TRUE




end event

event ue_antesguardar;Long	ll_Fila

This.TriggerEvent("ue_validaregistro")

//IF Message.DoubleParm <> -1 THEN	
//	FOR ll_Fila = 1 TO dw_1.RowCount()
//		dw_1.Object.plde_codigo[ll_Fila] = Integer(istr_Mant.Argumento[1])
//	NEXT
//END IF



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
	
	vinf.dw_1.DataObject = "dw_info_captura_archivocajas" 	
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

event ue_guardar;Integer	li_Agrupa, li_plantaori

If cbx_agrupa.Checked Then
	li_Agrupa = 1
Else
	li_Agrupa = 0
End If
//
If dw_1.AcceptText() = -1 Then Return

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")
Message.DoubleParm = 0
TriggerEvent("ue_antesguardar")

If Message.DoubleParm = 0 Then
	Delete from dbo.spro_cajasprod_trans;
	Commit;
	
	If wf_actualiza_db(False) Then
		pb_imprimir.Enabled = TRUE
		w_main.SetMicroHelp("Información Grabada.")
		
		If Busca_TipoPallet() Then
			MessageBox( "Atención", "No es Posible Grabar datos Por Falta de Tipo Pallet, Proceda por Mantención.", &
			Information!, Ok!)
			Delete From dbo.spro_cajasprod_trans;
			Commit;
			
			Close(w_captura_archivo_cajas_trans)
			Return;
		End If	

		li_plantaori 	= Integer(is_plantaori)
				
		DECLARE Traspaso_recepcion PROCEDURE FOR dbo.Fproc_Recepcion_tablas_trans	
			     @Agrupa 		= :li_Agrupa,
				  @pld_destino = :uo_SelPlantas.Codigo,
				  @pld_origen = :li_plantaori;
		EXECUTE Traspaso_recepcion;
				
		If sqlca.SQLCode = -1 Then
			F_ErrorBaseDatos(sqlca, "Lectura Procedimiento Fproc_Recepcion_tablas_trans")
			Return
		End If	
		 
		commit;
		MessageBox( "Atención", "El Proceso a Concluído Satisfactoriamente.", Information!, Ok!)
		pb_imprimir.Enabled	= True
   Else
	   w_main.SetMicroHelp("No se puede Grabar información.")
	   Message.DoubleParm = -1
		
	   Return
	End If
Else
	w_main.SetMicroHelp("No se puede Grabar información.")
   	Message.DoubleParm = -1
	pb_grabar.Enabled	 = False
   Return
End If
end event

event ue_modifica_detalle;call super::ue_modifica_detalle;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	//OpenWithParm(iw_mantencion, istr_mant)
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

event resize;call super::resize;pb_Archivo.x		=	pb_ins_det.x
pb_Archivo.y		=	pb_ins_det.y
pb_Archivo.Width	=	pb_ins_det.Width
pb_Archivo.Height	=	pb_ins_det.Height

dw_1.y					=	64 + st_encabe.Height
dw_1.height				=	This.WorkSpaceHeight() - dw_1.y - 41
end event

type dw_1 from w_mant_encab_deta`dw_1 within w_captura_archivo_cajas_trans
integer x = 82
integer y = 284
integer width = 2551
integer height = 1556
integer taborder = 100
string title = "Detalle de Captura"
string dataobject = "dw_mues_spro_cajasprod_trans_caja"
end type

type dw_2 from w_mant_encab_deta`dw_2 within w_captura_archivo_cajas_trans
boolean visible = false
integer x = 2857
integer y = 1468
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
//				FROM dbo.Existeparam
//				WHERE expa_identi = 1;
//				UPDATE dbo.Existeparam SET
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
//		FROM dbo.Existeparam
//		WHERE expa_identi = 1;
//		UPDATE dbo.Existeparam SET
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

type pb_nuevo from w_mant_encab_deta`pb_nuevo within w_captura_archivo_cajas_trans
integer x = 3109
integer y = 376
alignment htextalign = right!
end type

type pb_eliminar from w_mant_encab_deta`pb_eliminar within w_captura_archivo_cajas_trans
boolean visible = false
integer x = 2807
integer y = 1092
string picturename = "\Desarrollo 17\Imagenes\Botones\nulo.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\nulo-bn.png"
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
//			DECLARE revisa_saldos PROCEDURE FOR dbo.Exis_RevisaSaldosItemsdeGuia  
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

type pb_grabar from w_mant_encab_deta`pb_grabar within w_captura_archivo_cajas_trans
integer x = 3104
integer y = 564
end type

type pb_imprimir from w_mant_encab_deta`pb_imprimir within w_captura_archivo_cajas_trans
integer x = 3109
integer y = 736
end type

type pb_salir from w_mant_encab_deta`pb_salir within w_captura_archivo_cajas_trans
integer x = 3109
integer y = 1276
end type

type pb_ins_det from w_mant_encab_deta`pb_ins_det within w_captura_archivo_cajas_trans
boolean visible = false
integer x = 3150
integer y = 1524
end type

type pb_eli_det from w_mant_encab_deta`pb_eli_det within w_captura_archivo_cajas_trans
boolean visible = false
integer x = 3150
integer y = 1696
end type

type pb_buscar from w_mant_encab_deta`pb_buscar within w_captura_archivo_cajas_trans
boolean visible = false
integer x = 3109
integer y = 196
end type

type pb_archivo from picturebutton within w_captura_archivo_cajas_trans
integer x = 2802
integer y = 844
integer width = 302
integer height = 244
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos.png"
alignment htextalign = left!
end type

event clicked;String		Path, Nombre, PathPorDefault, ls_Nombre
Integer	li_valida, li_opcion = 1
Long		ll_rc, ll_fila, ll_Guia

ib_ok	= True

If dw_1.AcceptText() = -1 Then li_opcion = -1
If dw_1.ModIfiedCount() > 0 Then 
	li_opcion = 0
End If

CHOOSE CASE li_opcion
	CASE -1
		ib_ok = False
	CASE 0
		CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
			CASE 1
				Message.DoubleParm = 0
				This.triggerevent("ue_guardar")
				If message.doubleparm = -1 Then ib_ok = False
				Return
			CASE 3
				ib_ok	= False
				Return
		End CHOOSE
End CHOOSE

If ib_ok = False Then Return

dw_1.Reset()

DO
	RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, pathpordefault)
	If GetFileOpenName ("Carga de Archivo", path, nombre,  "csv", "Archivos Planos (*.csv),*.csv", pathpordefault)< 1 Then Return
	
	is_plantaori	= Mid(nombre, 10, 5)
	ls_nombre	= Mid(nombre, 1, 9)
	ll_Guia		= Long(Mid(nombre, 15, 8))
	
	If ls_nombre <> 'CajasProd' Then
		MessageBox("Atención","Archivo NO Corresponde a Packing",Information!,Ok!)
		Message.DoubleParm = 1
		Return
	End If	
	
	li_valida = dw_1.importfile(path)

	If li_valida = 0 Then		
		pb_salir.SetFocus()
		dw_1.Insertrow(0)
		Return
	ElseIf li_valida = -1 Then
		MessageBox("Error de Apertura","Ocurrió un error al utilizar el archivo",Information!,Ok!)
		Message.DoubleParm = 1
	Else				
		Message.DoubleParm = 2
		If dw_1.RowCount() = 0 Then 
			MessageBox("Atención", "No se cargó archivo exitosamente," + is_mensaje + ".", StopSign!, Ok!)
			FileClose(li_valida)
		Else
			For ll_Fila = 1 To dw_1.RowCount()
				dw_1.Object.capr_numgia[ll_Fila] = ll_Guia
				dw_1.Object.defe_guides[ll_Fila] = ll_Guia
			Next 
			pb_grabar.Enabled = True
		End If
	End If
	
LOOP WHILE Message.DoubleParm = 1
end event

type dw_3 from datawindow within w_captura_archivo_cajas_trans
boolean visible = false
integer x = 2656
integer y = 16
integer width = 197
integer height = 160
integer taborder = 40
boolean bringtotop = true
string title = "palletencab"
string dataobject = "dw_mues_palletencab_trans_caja"
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_captura_archivo_cajas_trans
boolean visible = false
integer x = 2866
integer y = 16
integer width = 197
integer height = 160
integer taborder = 60
boolean bringtotop = true
string title = "palletfruta"
string dataobject = "dw_mues_palletfruta_trans_caja"
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within w_captura_archivo_cajas_trans
boolean visible = false
integer x = 2661
integer y = 192
integer width = 197
integer height = 160
integer taborder = 90
boolean bringtotop = true
string title = "recfruprocee"
string dataobject = "dw_mues_recfruprocee_trans_caja"
borderstyle borderstyle = stylelowered!
end type

type dw_6 from datawindow within w_captura_archivo_cajas_trans
boolean visible = false
integer x = 2871
integer y = 192
integer width = 197
integer height = 160
integer taborder = 90
boolean bringtotop = true
string title = "recfruproced"
string dataobject = "dw_mues_recfruproced_trans_caja"
borderstyle borderstyle = stylelowered!
end type

type cbx_agrupa from checkbox within w_captura_archivo_cajas_trans
integer x = 1838
integer y = 80
integer width = 613
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
string text = "Agrupa a Bultos"
end type

type st_encabe from statictext within w_captura_archivo_cajas_trans
integer x = 82
integer y = 16
integer width = 2551
integer height = 232
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_7 from datawindow within w_captura_archivo_cajas_trans
boolean visible = false
integer x = 2661
integer y = 360
integer width = 197
integer height = 160
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_cajasprod_trans_caja"
borderstyle borderstyle = stylelowered!
end type

type dw_agrupa from datawindow within w_captura_archivo_cajas_trans
boolean visible = false
integer x = 2866
integer y = 360
integer width = 197
integer height = 160
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_cajasprod_trans_agrupado"
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_captura_archivo_cajas_trans
integer x = 146
integer y = 80
integer width = 302
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

type uo_selplantas from uo_seleccion_plantas within w_captura_archivo_cajas_trans
event destroy ( )
integer x = 421
integer y = 68
integer height = 100
integer taborder = 40
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

