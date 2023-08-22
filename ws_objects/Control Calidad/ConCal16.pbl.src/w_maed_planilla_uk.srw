$PBExportHeader$w_maed_planilla_uk.srw
$PBExportComments$Encabezado de Ingreso de Inspección en Destino UK
forward
global type w_maed_planilla_uk from w_mant_encab_deta_csd
end type
end forward

global type w_maed_planilla_uk from w_mant_encab_deta_csd
integer width = 3602
integer height = 1980
string title = "PLANILLA DE INSPECCIÓN DESTINO U.K."
string menuname = ""
boolean resizable = false
event ue_carga_detalle ( )
end type
global w_maed_planilla_uk w_maed_planilla_uk

type variables
OleObject	myoleobject
string is_Docname, is_Named

uo_Variedades	iuo_Variedades
uo_Productores	iuo_Productores
uo_Embalajesprod	iuo_Embalajes
uo_Etiquetas	iuo_Etiquetas
uo_Recibidores	iuo_Recibidores
uo_Puertos		iuo_Puertos
uo_Naves			iuo_Naves

end variables

forward prototypes
protected function integer wf_modifica ()
public function boolean buscaproductor ()
public function boolean buscapuerto ()
public function boolean buscapuertodet (string as_puerto)
public function boolean buscarecibidordet (string as_recibidor, integer as_codcli)
public function boolean buscarecibidor (integer as_codcli)
public function boolean buscanavedet (string as_nave, string as_tipotr)
public function boolean buscanave ()
public function boolean buscaetiquetadet (string as_etiqueta)
public subroutine buscaetiqueta (integer ai_fila)
public function boolean buscavariedad (integer ai_fila)
public subroutine buscaembalaje (integer ai_fila)
end prototypes

event ue_carga_detalle();
Integer	li_Coneccion, li_Detalle, li_Encab, li_TamLote, li_Ano,li_ProdCod,li_Vari, li_Resta, li_Null
Long		ll_fila, ll_Columna, ll_Pallet 			
Dec{1} 	ld_CalEmb, ld_CalCal, ld_CalCon, ld_Temp, ld_deslev,ld_Desmod, ld_DesAlt,&
			ld_Conden, ld_Pardea, ld_Desgra,	ld_FruBla,ld_DesPed, ld_PaSeco, ld_PasHum, ld_Traslu,ld_Machuca,ld_Danso2
String 	ls_GuaLot, ls_GuaCaj, ls_Emba, ls_FecEmb, ls_Etiq, ls_Nave, ls_Puerto, ls_recibidor
Date		ld_FecEmb

SetNull(li_Null)
SetPointer(HourGlass!)
myoleobject			= CREATE OLEObject 
iuo_Variedades		= CREATE	uo_Variedades
iuo_Productores	= CREATE	uo_Productores
iuo_Embalajes		= CREATE	uo_Embalajesprod

li_Coneccion = myoleobject.ConnectToObject(is_Docname) 

IF li_Coneccion = 0 THEN 
	li_Encab = dw_2.InsertRow(0)
	FOR  ll_Fila = 3 TO 11		
		dw_2.Object.clie_codigo[li_Encab]	=	81
		IF ll_Fila= 3 	THEN dw_2.Object.cpde_numero[li_Encab]	=	Long(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,54).value)
		IF ll_Fila= 11 THEN dw_2.Object.cpde_fecins[li_Encab]	=	Date(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,32).value)
		
		dw_2.Object.nave_tipotr[li_Encab]	=	'M'
		
		IF ll_Fila = 8 	THEN 
			ls_Nave	=	String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,8).value)
			IF Trim(ls_Nave)  <> Trim(istr_Mant.Argumento[5]) THEN
				IF BuscaNaveDet(ls_Nave,'M') = False THEN
					dw_2.Object.nave_nombre[li_Encab]	=	ls_Nave
				ELSE
					dw_2.Object.nave_codigo[li_Encab]	=	Integer(istr_Mant.Argumento[5])
					dw_2.Object.nave_nombre[li_Encab]	=	ls_Nave
				END IF
			ELSE
				dw_2.Object.nave_codigo[li_Encab]	=	Integer(istr_Mant.Argumento[4])
				dw_2.Object.nave_nombre[li_Encab]	=	ls_Nave
			END IF			
		END IF
		
		IF ll_Fila = 11 	THEN 
			ls_Puerto = String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,8).value)
			IF Trim(ls_Puerto)  <> Trim(istr_Mant.Argumento[6]) THEN
				IF BuscaPuertoDet(ls_Puerto) = False THEN
					dw_2.Object.puer_nombre[li_Encab]	=	ls_Puerto
				ELSE
					dw_2.Object.puer_codigo[li_Encab]	=	Integer(istr_Mant.Argumento[7])
					dw_2.Object.puer_nombre[li_Encab]	=	ls_Puerto
				END IF	
			ELSE
				dw_2.Object.puer_nombre[li_Encab]	=	ls_Puerto
				dw_2.Object.puer_codigo[li_Encab]	=	Integer(istr_Mant.Argumento[7])				
			END IF			
		END IF 
		
		IF ll_Fila= 8 	THEN 
			ls_recibidor = String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,50).value)
			IF Trim(ls_recibidor)  <> Trim(istr_Mant.Argumento[8]) THEN
				IF BuscaRecibidorDet(ls_recibidor,81) = False THEN
					dw_2.Object.reci_nombre[li_Encab]	=	ls_recibidor	
				ELSE
					dw_2.Object.reci_codigo[li_Encab]	=	Integer(istr_Mant.Argumento[9])
					dw_2.Object.reci_nombre[li_Encab]	=	ls_recibidor
				END IF	
			ELSE
				dw_2.Object.reci_codigo[li_Encab]	=	Integer(istr_Mant.Argumento[9])
				dw_2.Object.reci_nombre[li_Encab]	=	ls_recibidor
			END IF			
		END IF
		
		IF ll_Fila= 7	THEN dw_2.Object.cpde_paisde[li_Encab]	=	String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,8).value)
		IF ll_Fila= 9 	THEN dw_2.Object.cpde_bodega[li_Encab]	=	String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,8).value)
		IF ll_Fila= 10	THEN dw_2.Object.cpde_conten[li_Encab]	=	String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,8).value)
		IF ll_Fila= 7 	THEN dw_2.Object.cpde_lugins[li_Encab]	=	String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,32).value)
		IF ll_Fila= 8 	THEN dw_2.Object.cpde_fecarr[li_Encab]	=	Date(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,32).value)
		IF ll_Fila= 9 	THEN dw_2.Object.cpde_fecdes[li_Encab]	=	Date(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,32).value)
		IF ll_Fila= 10 THEN dw_2.Object.cpde_fecfum[li_Encab]	=	Date(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,32).value)
		IF ll_Fila= 10 THEN dw_2.Object.cpde_inspec[li_Encab]	=	String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,50).value)	
	NEXT
	
	FOR  ll_Fila = 19 TO 43
		ll_Pallet	=	Long(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,3).value)
		IF	ll_Pallet > 0 OR NOT IsNull(ll_Pallet) THEN
			li_Detalle = dw_1.InsertRow(0)
			ll_Columna = 0  
			dw_1.Object.clie_codigo[li_Detalle]	=	81
			dw_1.Object.cpde_numero[li_Detalle]	=	dw_2.Object.cpde_numero[li_Encab]
			dw_1.Object.cpde_fecins[li_Detalle]	=	dw_2.Object.cpde_fecins[li_Encab]
			dw_1.Object.cpdd_secuen[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,1).value)
			li_TamLote = Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,2).value)
			IF li_TamLote = 0  or IsNull(li_TamLote) THEN
				dw_1.Object.cpdd_tamlot[li_Detalle]	=	dw_1.Object.cpdd_tamlot[li_Detalle  - 1]
			ELSE
				dw_1.Object.cpdd_tamlot[li_Detalle]	=	li_TamLote
			END IF
			
			dw_1.Object.cpdd_nropal[li_Detalle]	=	Long(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,3).value)
			ld_Temp = Dec(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,4).value)
			IF ld_Temp > 10 THEN
				ld_Temp = ld_Temp/10
			END IF 
			dw_1.Object.cpdd_temper[li_Detalle]	= ld_Temp
			
			li_ProdCod  = Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,5).value)
			
			IF li_ProdCod  <> Integer(istr_Mant.Argumento[1]) THEN
				IF Not iuo_Productores.Existe(Long(li_ProdCod),True,Sqlca) THEN
					DO WHILE Buscaproductor( ) = False 
						MessageBox("Atención","Debe Seleccionar un productor",Exclamation!)
					LOOP	
					li_ProdCod	=	Integer(istr_Mant.Argumento[1])
				ELSE
					istr_Mant.Argumento[1]	=	String(li_ProdCod)
				END IF
			END IF
			
			dw_1.Object.prod_codigo[li_Detalle]	=	li_ProdCod				
			dw_1.Object.espe_codigo[li_Detalle]	=	11
			li_Vari = Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,6).value)
			
			IF li_Vari  <> Integer(istr_Mant.Argumento[2]) THEN
				IF Not iuo_Variedades.Existe(11,li_Vari,True,Sqlca) THEN
						dw_1.Object.vari_nombre[li_Detalle]	=	String(li_Null)
				ELSE
					istr_Mant.Argumento[2]	=	String(li_Vari)
					dw_1.Object.vari_codigo[li_Detalle]	=	iuo_Variedades.Variedad
					dw_1.Object.vari_nombre[li_Detalle]	=	iuo_Variedades.NombreVariedad
				END IF
			ELSE
				dw_1.Object.vari_codigo[li_Detalle]	=	li_Vari
				dw_1.Object.vari_nombre[li_Detalle]	=	iuo_Variedades.NombreVariedad
			END IF			
				
			dw_1.Object.cpdd_calibr[li_Detalle]	=	String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,7).value)
			
			ls_Emba = String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,8).value)
			ls_Emba = Mid(ls_Emba,1,1) +  Mid(ls_emba,3,3)
			
			IF Trim(ls_Emba)  <> Trim(istr_Mant.Argumento[3]) THEN
				IF Not iuo_Embalajes.Existe(81,Trim(ls_Emba),True,Sqlca) THEN
					dw_1.Object.emba_nombre[li_Detalle]	=	ls_Emba
				ELSE
					dw_1.Object.emba_codigo[li_Detalle]	=	iuo_Embalajes.Codigo
					dw_1.Object.emba_nombre[li_Detalle]	=	iuo_Embalajes.Nombre
					istr_Mant.Argumento[3]	=	Trim(ls_Emba)
				END IF
			ELSE
				dw_1.Object.emba_codigo[li_Detalle]	=	Trim(ls_Emba)
				dw_1.Object.emba_nombre[li_Detalle]	=	iuo_Embalajes.Nombre
			END IF
						
			ls_Etiq	=	String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,9).value)
			IF Trim(ls_Etiq)  <> Trim(istr_Mant.Argumento[11]) THEN
				IF  BuscaEtiquetaDet(ls_Etiq) = False THEN
					dw_1.Object.etiq_nombre[li_Detalle]	=	ls_Etiq
				ELSE
					dw_1.Object.etiq_codigo[li_Detalle]	=	Integer(Istr_Mant.Argumento[10])
					dw_1.Object.etiq_nombre[li_Detalle]	=	ls_Etiq
				END IF
			ELSE
				dw_1.Object.etiq_codigo[li_Detalle]	=	Integer(Istr_Mant.Argumento[10])
				dw_1.Object.etiq_nombre[li_Detalle]	=	ls_Etiq
			END IF
			
			ls_FecEmb = String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,10).value)
			
			IF Integer(Mid(ls_FecEmb,1,1)) > 2 THEN
				li_Ano = Integer(String(Today(),'yyyy'))
			ELSE
				li_Ano = Integer(String(Today(),'yyyy')) - 1
			END IF 
			ld_FecEmb	= Date(Mid(ls_FecEmb,2,2) + '/' +  Mid(ls_FecEmb,1,1) + '/' + String(li_Ano))
			dw_1.Object.cpdd_fecemb[li_Detalle]	=	ld_FecEmb			
			dw_1.Object.cpdd_detpal[li_Detalle]	=	String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,11).value)
			dw_1.Object.cpdd_rotula[li_Detalle]	=	String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,12).value)
			dw_1.Object.cpdd_matint[li_Detalle]	=	String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,13).value)
			dw_1.Object.cpdd_nropaq[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,14).value)
			
			ld_CalEmb = Dec(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,15).value)
			IF ld_CalEmb > 10 THEN 
				ld_CalEmb = ld_CalEmb/10
			END IF
			
			dw_1.Object.cpdd_calemb[li_Detalle]	=	ld_CalEmb
			dw_1.Object.cpdd_cbsoco[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,16).value)
			dw_1.Object.cpdd_cbmayo[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,17).value)
			dw_1.Object.cpdd_cbbaya[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,18).value)
			dw_1.Object.cpdd_cbmeno[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,19).value)
			dw_1.Object.cpdd_tbmayo[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,20).value)
			dw_1.Object.cpdd_tbayxl[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,21).value)
			dw_1.Object.cpdd_tbayae[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,22).value)
			dw_1.Object.cpdd_tbayar[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,23).value)
			dw_1.Object.cpdd_tba300[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,24).value)
			dw_1.Object.cpdd_tbmeno[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,25).value)
			dw_1.Object.cpdd_racnor[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,26).value)
			dw_1.Object.cpdd_racdef[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,27).value)
			dw_1.Object.cpdd_racapr[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,28).value)
			dw_1.Object.cpdd_rasbpe[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,29).value)
			dw_1.Object.cpdd_rapeno[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,30).value)
			dw_1.Object.cpdd_rapeba[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,31).value)
			dw_1.Object.cpdd_defman[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,32).value)
			dw_1.Object.cpdd_defgso[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,33).value)
			dw_1.Object.cpdd_defres[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,34).value)
			dw_1.Object.cpdd_grbrix[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,35).value)
			
			ld_CalCal = Dec(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,36).value)
			IF ld_CalCal > 10 THEN
				ld_CalCal = ld_CalCal/10
			END IF 
			dw_1.Object.cpdd_calcal[li_Detalle]	=	ld_CalCal	
			
			ld_deslev = Dec(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,37).value)
			IF ld_deslev > 100 THEN
				ld_deslev = ld_deslev/10
			END IF
			dw_1.Object.cpdd_deslev[li_Detalle]	=	ld_deslev
			
			ld_Desmod = Dec(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,38).value)
			IF ld_Desmod > 100 THEN
				ld_Desmod = ld_Desmod/10
			END IF
			dw_1.Object.cpdd_desmod[li_Detalle]	=	ld_Desmod
			
			ld_DesAlt = Dec(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,39).value)		
			IF ld_DesAlt > 100 THEN
				ld_DesAlt = ld_DesAlt/10
			END IF
			
			dw_1.Object.cpdd_desalt[li_Detalle]	=	ld_DesAlt
			
			dw_1.Object.cpdd_pudbay[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,40).value)
			dw_1.Object.cpdd_pudnid[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,41).value)
			dw_1.Object.cpdd_pudrac[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,42).value)
			
			ld_Conden = Dec(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,43).value)
			dw_1.Object.cpdd_conden[li_Detalle]	=	ld_Conden / 10
			
			ld_Pardea = Dec(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,44).value)
			dw_1.Object.cpdd_pardea[li_Detalle]	=	ld_Pardea/10
			
			ld_Danso2 = Dec(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,45).value)
			dw_1.Object.cpdd_danso2[li_Detalle]	=	ld_Danso2/10
			
			ld_Machuca = Dec(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,46).value)
			dw_1.Object.cpdd_machuc[li_Detalle]	=	ld_Machuca/10
			
			ld_Traslu = Dec(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,47).value)
			dw_1.Object.cpdd_traslu[li_Detalle]	=	ld_Traslu/10
			
			dw_1.Object.cpdd_watber[li_Detalle]	=	Integer(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,48).value)
			
			ld_PasHum = Dec(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,49).value)
			dw_1.Object.cpdd_parhum[li_Detalle]	=	ld_PasHum/10
			
			ld_PaSeco = Dec(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,50).value)
			dw_1.Object.cpdd_paseco[li_Detalle]	=	ld_PaSeco/10
			
			ld_DesPed = Dec(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,51).value)
			dw_1.Object.cpdd_desped[li_Detalle]	=	ld_DesPed/10
			
			ld_FruBla = Dec(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,52).value)
			dw_1.Object.cpdd_frubla[li_Detalle]	=	ld_FruBla/10
			
			ld_Desgra = Dec(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,53).value)
			dw_1.Object.cpdd_desgra[li_Detalle]	=	ld_Desgra/10
			
			ld_CalCon = Dec(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,54).value)
			IF ld_CalCon > 10 THEN
				ld_CalCon = ld_CalCon/10
			END IF 
			dw_1.Object.cpdd_calcon[li_Detalle]	=	ld_CalCon
			ls_GuaCaj = String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,55).value)
			IF Trim(ls_GuaCaj) = 'SI' THEN 
				dw_1.Object.cpdd_guacaj[li_Detalle]	=	1
			ELSEIF Trim(ls_GuaCaj) = 'NO' THEN
				dw_1.Object.cpdd_guacaj[li_Detalle]	=	0
			END IF
			ls_GuaLot = String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,56).value)
			IF Trim(ls_GuaLot) = 'SI' THEN 
				dw_1.Object.cpdd_gualot[li_Detalle]	=	1
			ELSEIF Trim(ls_GuaLot) = 'NO' THEN
				dw_1.Object.cpdd_gualot[li_Detalle]	=	0
			END IF	
			dw_1.Object.cpdd_nfotos[li_Detalle]	=	String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,57).value)
			dw_1.Object.cpdd_observ[li_Detalle]	=	String(myoleobject.application.workbooks(1).worksheets(1).cells(ll_Fila,58).value)
		END IF
	NEXT
END IF 
myoleobject.application.workbooks.Close()
myoleobject.disconnectobject()
Destroy myoleobject
Destroy iuo_Variedades
Destroy iuo_Productores
Destroy iuo_Embalajes	
SetPointer(Arrow!)

pb_grabar.Enabled		= True
pb_imprimir.Enabled	= True
end event

protected function integer wf_modifica ();IF dw_1.AcceptText() = -1 THEN RETURN -1
IF dw_2.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0
IF dw_2.ModifiedCount() > 0 THEN RETURN 0
RETURN 1
end function

public function boolean buscaproductor ();Boolean lb_Boolean = False

OpenWithParm(w_busc_productores, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[1] <> "" THEN
	istr_mant.argumento[1] = istr_busq.argum[1]	
	lb_Boolean = True	
END IF

RETURN lb_Boolean

end function

public function boolean buscapuerto ();Boolean	lb_Boolean = False

	istr_busq.argum[1]	=	'900'
	
	OpenWithParm(w_busc_puertos, istr_busq)
	
	istr_busq	= Message.PowerObjectParm
	
	IF istr_busq.argum[2] <> "" THEN
		dw_2.Object.puer_codigo[1] = Integer(istr_busq.argum[3])
		dw_2.Object.puer_nombre[1] = istr_busq.argum[2]
		lb_Boolean = True	
	END IF

RETURN lb_Boolean
end function

public function boolean buscapuertodet (string as_puerto);Boolean	lb_Boolean = False
Integer	li_Contador
String	ls_Puerto

ls_Puerto	= '%' + As_Puerto + '%'

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.Puertos
	WHERE	puer_nombre LIKE (:ls_Puerto)
	AND	puer_codigo <=  900;
	
IF Sqlca.SqlCode = -1 THEN
	F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Puerto")
ELSEIF sqlca.SQLCode = 100  OR li_Contador <> 1 THEN
		istr_busq.argum[1]	=	'900'
		istr_busq.argum[2]	=	ls_Puerto
		
		OpenWithParm(w_busc_puertos_archivo_excel, istr_busq)
		
		istr_busq	= Message.PowerObjectParm
		
		IF istr_busq.argum[3] <> "" THEN
			istr_mant.argumento[7] = istr_busq.argum[3]	
			istr_mant.argumento[6] = as_Puerto
			lb_Boolean = True	
		END IF
ELSE
	SELECT	puer_codigo
		INTO	:li_Contador
		FROM	dba.puertos
		WHERE	puer_nombre LIKE (:ls_Puerto)
		AND 	puer_codigo <= 900;
		
	IF Sqlca.SqlCode = -1 THEN
		F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Puerto")
	ELSEIF sqlca.SQLCode = 0 THEN
		istr_mant.argumento[7] = String(li_Contador)
		istr_mant.argumento[6] = as_Puerto
	END IF
	lb_Boolean = True
END IF

RETURN lb_Boolean
end function

public function boolean buscarecibidordet (string as_recibidor, integer as_codcli);Boolean	lb_Boolean = False
Integer	li_Contador
String	ls_Recibidor

ls_Recibidor	= '%' + As_Recibidor + '%'

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.recibidores
	WHERE	reci_nombre LIKE (:ls_Recibidor) ;
	
IF Sqlca.SqlCode = -1 THEN
	F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Recibidores")
ELSEIF sqlca.SQLCode = 100  OR li_Contador <> 1 THEN
		istr_busq.argum[1]	=	String(as_CodCli)
		istr_busq.argum[2]	=	ls_Recibidor
		OpenWithParm(w_busc_recibidores_plan_dest, istr_busq)
		
		istr_busq	= Message.PowerObjectParm
		
		IF istr_busq.argum[3] <> "" THEN
			istr_mant.argumento[9] = istr_busq.argum[3]	
			istr_mant.argumento[8] = As_Recibidor
			lb_Boolean = True	
		END IF
ELSE
	SELECT	reci_Codigo
		INTO	:li_Contador
		FROM	dba.recibidores
		WHERE	reci_nombre LIKE (:ls_Recibidor)	;
		
	IF Sqlca.SqlCode = -1 THEN
		F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Recibidores")
	ELSEIF sqlca.SQLCode = 0 THEN
		istr_mant.argumento[9] = String(li_Contador)
		istr_mant.argumento[8] = As_Recibidor
	END IF
	lb_Boolean = True
END IF

RETURN lb_Boolean
end function

public function boolean buscarecibidor (integer as_codcli);Boolean	lb_Boolean = False

istr_busq.argum[1]	=	String(as_CodCli)

OpenWithParm(w_busc_recibidores, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	
	dw_2.Object.reci_codigo[1] = Integer(istr_busq.argum[1])
	dw_2.Object.reci_nombre[1] = istr_busq.argum[2]
	lb_Boolean = True	
END IF
		

RETURN lb_Boolean
end function

public function boolean buscanavedet (string as_nave, string as_tipotr);Boolean	lb_Boolean = False
Integer	li_Contador
String	ls_Nave

ls_Nave	= '%' + as_Nave + '%'

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.naves
	WHERE	nave_nombre LIKE (:ls_Nave)
	AND	nave_tipotr = :as_Tipotr;
	
IF Sqlca.SqlCode = -1 THEN
	F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Naves")
ELSEIF sqlca.SQLCode = 100  OR li_Contador <> 1 THEN
	
		istr_busq.argum[1]	=	as_Tipotr
		istr_busq.argum[2]	=	ls_Nave
		OpenWithParm(w_busc_naves, istr_busq)
		
		istr_busq	= Message.PowerObjectParm
		
		IF istr_busq.argum[3] <> "" THEN
			istr_mant.argumento[4] = istr_busq.argum[3]	
			istr_mant.argumento[5] = as_Nave
			lb_Boolean = True	
		END IF
ELSE
	SELECT	Nave_Codigo
		INTO	:li_Contador
		FROM	dba.naves
		WHERE	nave_nombre LIKE (:ls_Nave)
		AND 	nave_tipotr =:as_Tipotr;
		
	IF Sqlca.SqlCode = -1 THEN
		F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Naves")
	ELSEIF sqlca.SQLCode = 0 THEN
		istr_mant.argumento[4] = String(li_Contador)
		istr_mant.argumento[5] = as_Nave
	END IF
	lb_Boolean = True
END IF

RETURN lb_Boolean
end function

public function boolean buscanave ();Boolean	lb_Boolean = False
Integer	li_Contador
String	ls_Nave

istr_busq.argum[1]	=	dw_2.Object.nave_tipotr[1]
istr_busq.argum[2] = '*'
		
OpenWithParm(w_busc_naves, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[3] <> "" THEN
	dw_2.Object.nave_codigo[1] = Integer(istr_busq.argum[3])
	dw_2.Object.nave_nombre[1] = istr_busq.argum[4]
	lb_Boolean = True	
END IF

RETURN lb_Boolean
end function

public function boolean buscavariedad (integer ai_fila);Boolean lb_Boolean = False
istr_busq.argum[1]	=	'81'
istr_busq.argum[2]	=	'11'
OpenWithParm(w_busc_variedades, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[1] <> "" THEN
	dw_1.Object.vari_codigo[ai_Fila] = Integer(istr_busq.argum[4])
	dw_1.Object.vari_nombre[ai_Fila] = istr_busq.argum[5]
	lb_Boolean = True	
END IF

RETURN lb_Boolean
end function

public subroutine buscaembalaje (integer ai_fila);Boolean lb_Boolean = False

istr_busq.argum[1] = '81'

OpenWithParm(w_busc_embalajes, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	dw_1.Object.emba_codigo[ai_fila] = istr_busq.argum[2]
	dw_1.Object.emba_nombre[ai_fila] = istr_busq.argum[3]
END IF


end subroutine

public function boolean buscaetiquetadet (integer ai_codcli, string as_etiqueta);Boolean	lb_Boolean = False
Integer	li_Contador
String	ls_Etiqueta, ls_Nombre

ls_Etiqueta	= '%' + as_Etiqueta + '%'

SELECT	Count(*)
	INTO	:li_Contador
	FROM	dba.etiquetas
	WHERE	etiq_abrevi LIKE (:ls_Etiqueta);
	
IF Sqlca.SqlCode = -1 THEN
	F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Etiqueta")
ELSEIF sqlca.SQLCode = 100  OR li_Contador <> 1 THEN
	
//		istr_busq.argum[1]	=	String(ai_CodCli)
//		istr_busq.argum[2]	=	ls_Etiqueta
//		OpenWithParm(w_busc_Etiquetas_plan_dest, istr_busq)
//		
//		istr_busq	= Message.PowerObjectParm
//		
//		IF istr_busq.argum[3] <> "" THEN
//			istr_mant.argumento[10] = istr_busq.argum[3]	
//			istr_mant.argumento[11] = as_Etiqueta
//			lb_Boolean = True	
//		END IF
	lb_Boolean = True	
ELSE
	SELECT	etiq_codigo, etiq_nombre
		INTO	:li_Contador,:ls_Nombre
		FROM	dba.etiquetas
		WHERE	etiq_abrevi LIKE (:ls_Etiqueta);
		
	IF Sqlca.SqlCode = -1 THEN
		F_ErrorBaseDatos(Sqlca,"Lectura de Tabla de Etiqueta")
	ELSEIF sqlca.SQLCode = 0 THEN
		istr_mant.argumento[10] = String(li_Contador)
		istr_mant.argumento[11] = as_Etiqueta
		istr_mant.argumento[12] = ls_Nombre
	END IF
	lb_Boolean = True
END IF

RETURN lb_Boolean
end function

public subroutine buscaetiqueta (integer ai_codcli, integer ai_fila);Boolean	lb_Boolean = False
Integer	li_Contador
String	ls_Etiqueta, ls_Nombre

	
istr_busq.argum[3]	=	String(ai_CodCli)
OpenWithParm(w_busc_Etiquetas, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[3] <> "" THEN
	dw_1.Object.etiq_codigo[ai_Fila] = Integer(istr_busq.argum[1])
	dw_1.Object.etiq_nombre[ai_Fila] = istr_busq.argum[2]
	
END IF


end subroutine

on w_maed_planilla_uk.create
call super::create
end on

on w_maed_planilla_uk.destroy
call super::destroy
end on

event ue_seleccion;Integer li_Valor

pb_imprimir.Enabled	=	False

dw_1.Reset()
dw_2.Reset()

istr_Mant.Argumento[1]	=	'0' // Argumento que almacena Cod. Productor
istr_Mant.Argumento[2]	=	'0' // Argumento que almacena Cod. Variedad
istr_Mant.Argumento[3]	=	''  // Argumento que almacena Cod. Embalaje
istr_Mant.Argumento[4]	=	''  // Argumento que almacena Nombre Nave
istr_Mant.Argumento[5]	=	'0' // Argumento que almacena Cod. Nave
istr_Mant.Argumento[6]	=	''  // Argumento que almacena Nombre Puerto
istr_Mant.Argumento[7]	=	'0' // Argumento que almacena Cod. Puerto
istr_mant.argumento[8]  = 	''	 // Argumento que almacena Nombre Recibidor
istr_mant.argumento[9]  = 	'0' // Argumento que almacena Codigo Recibidor	
istr_mant.argumento[10] = 	''	 // Argumento que almacena Nombre Etiqueta
istr_mant.argumento[11] = 	'0' // Argumento que almacena Codigo Etiqueta
istr_mant.argumento[12] = 	'' // Argumento que almacena nombre Etiqueta


li_Valor = GetFileOpenName("Select File",is_Docname, is_Named, "XLS", &
    + "Excel Files (*.xls),*.xls," &
    + "Excel Files (*.xls),*.xls")
	 
IF li_Valor < 1 THEN
	MessageBox("Atención","Error al Abrir Archivo",Exclamation!)
	RETURN  
ELSE
	This.TriggerEvent("ue_carga_detalle")
END IF

SetPointer(Arrow!)
end event

event ue_recuperadatos;//Long 		ll_fila_e, ll_fila_d, ll_fila_f, respuesta
//String	ls_Usuario
//Integer	li_Grupo
//ls_Usuario	=	Upper(Gstr_Us.Nombre)
//
//DO
//	dw_2.SetRedraw(False)
//	dw_2.Reset()
//	
//	ll_fila_e	=	dw_2.Retrieve(Integer(istr_mant.argumento[1]), &
//										  Integer(istr_mant.argumento[3]))
//	
//	IF ll_fila_e = -1 THEN
//		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
//										Information!, RetryCancel!)
//	ELSE
//		idwc_agronomos.SetTransObject(sqlca)			
//	   idwc_agronomos.Retrieve(0,dw_2.Object.zona_codigo[1])	
//		DO			
//   		ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[1]), &
//											    Integer(istr_mant.argumento[3]))
//
//			IF ll_fila_d = -1 THEN				
//				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
//												Information!, RetryCancel!)
//			ELSE
//				pb_grabar.Enabled		= True
//				pb_ins_det.Enabled	= True
//			END IF	
//			
//		IF ll_fila_d > 0 THEN	
//			
//			li_Grupo = BuscaGrupo(ls_Usuario)
//			
//			IF (li_Grupo = 6 )	OR (li_Grupo = 1)  THEN 				
//				pb_imprimir.Enabled	= True
//				dw_1.SetRow(1)
//				dw_1.SelectRow(1,False)
//				dw_1.SetFocus()				
//			ELSE
//				dw_2.Enabled 				=	False
//				dw_1.Enabled				=	False				
//				istr_mant.Solo_Consulta =	True 		
//				
//			END IF 					
//		ELSE
//			pb_ins_det.SetFocus()				
//		END IF
//			
//		LOOP WHILE respuesta = 1
//		
//		HabilitaEncab(False)
//
//		IF respuesta = 2 THEN Close(This)
//	END IF
//	dw_2.SetRedraw(True)
//LOOP WHILE respuesta = 1
//
//IF respuesta = 2 THEN Close(This)
//		
end event

event ue_nuevo;//Long		ll_modif1, ll_modif2, ll_modif3
//
//ib_ok	= True
//
//istr_busq.argum[1]		=	""
//istr_busq.argum[2]		=	""
//istr_Mant.Argumento[3]	=	""
//
//istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta
//
//IF Not istr_mant.Solo_Consulta THEN
//	CHOOSE CASE wf_modifica()			
//		CASE -1
//			ib_ok = False			
//		CASE 0
//			ll_modif1	=	dw_1.GetNextModified(0, Primary!)
//			ll_modif2	=	dw_2.GetNextModified(0, Primary!)	
//
//			IF dw_1.RowCount() > 0 THEN
//				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
//					CASE 1
//						Message.DoubleParm = 0
//						This.TriggerEvent("ue_guardar")
//						IF message.DoubleParm = -1 THEN ib_ok = False
//					CASE 3
//						ib_ok	= False
//						RETURN
//				END CHOOSE
//			END IF
//	END CHOOSE
//END IF
//
//IF Not ib_ok THEN RETURN
//
//dw_1.Reset()
//
//HabilitaEncab(True)
//
//pb_eliminar.Enabled	=	False
//pb_eli_det.Enabled	=	False
//pb_ins_det.Enabled	=	False
//pb_grabar.Enabled		=	False
//pb_imprimir.Enabled	=	False
//dw_2.Enabled			=	True
//
//dw_2.SetRedraw(False)
//dw_2.Reset()
//dw_2.InsertRow(0)
//dw_2.SetRedraw(True)
//
//dw_2.SetColumn("plde_codigo")
//dw_2.SetItem(1, "clie_codigo",gi_codexport)
//dw_2.SetItem(1, "zona_codigo",gi_CodZona)
//dw_2.SetItem(1, "plde_codigo",gi_CodPlanta)
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;//String	ls_Calificacion, ls_CalCalidad, ls_CalCondicion, ls_CalEmbalaje, &
//			ls_Mensaje,ls_Colu[]
//Integer	li_Cont
//
//
//IF il_fila > 0 THEN
//	pb_eliminar.Enabled	= True
//	pb_grabar.Enabled		= True
//END IF
//
//IF dw_1.RowCount()	>	0	THEN	
//	
//	IF dw_1.Object.cctd_resolu[il_fila] = 'R' THEN 
//		
//		IF Isnull(dw_1.Object.ccda_secuen[il_fila]) OR &
//			dw_1.Object.ccda_secuen[il_fila] = 0 THEN 
//			MessageBox("Atención","Falta Ingresar Causal de Objeción",Exclamation!)
//						dw_1.SetColumn("ccda_secuen")
//		END IF
//		
//		IF Isnull(dw_1.Object.cctd_embal1[il_fila]) OR &
//			dw_1.Object.cctd_embal1[il_fila] = 0 THEN 	
//	
//			IF Isnull(dw_1.Object.cctd_calid1[il_fila]) &
//				OR dw_1.Object.cctd_calid1[il_fila] = 0 THEN 
//	
//				IF Isnull(dw_1.Object.cctd_condi1[il_fila]) OR &
//					dw_1.Object.cctd_condi1[il_fila] = 0 THEN 
//						MessageBox("Atención","Falta Ingresar Causal de Rechazo",Exclamation!)
//						dw_1.SetColumn("cctd_embal1")
//				END IF
//			END IF
//		END IF
//	END IF
//	
//	IF Isnull(dw_1.Object.cctd_folpla[il_fila]) OR &
//			dw_1.Object.cctd_folpla[il_fila] = 0 THEN 
//				MessageBox("Atención","Falta Ingresar Folio Planilla",Exclamation!)
//				dw_1.SetColumn("cctd_folpla")
//		
//	ELSE
//	
//		il_fila = dw_1.InsertRow(0)
//		dw_1.Setfocus()
//		dw_1.ScrollToRow(il_fila)
//		dw_1.SetRow(il_fila)
//		dw_1.SetColumn("cctd_tipins")
//		
//		IF dw_1.RowCount() > 0 THEN
//			dw_1.Object.cctd_secuen[il_fila] = dw_1.RowCount()
//		ELSE
//			dw_1.Object.cctd_secuen[il_fila] = 1
//		END IF	
//	END IF
//ELSE
//	IF dw_1.RowCount()	=	0	THEN il_fila = dw_1.InsertRow(0)	
//	
//	dw_1.Setfocus()
//	dw_1.ScrollToRow(il_fila)
//	dw_1.SetRow(il_fila)
//	dw_1.SetColumn("cctd_tipins")
//	
//	IF dw_1.RowCount() > 0 THEN
//		dw_1.Object.cctd_secuen[il_fila] = dw_1.RowCount()
//	ELSE
//		dw_1.Object.cctd_secuen[il_fila] = 1
//	END IF	
//		
//END IF
//dw_2.SetItem(1, "zona_codigo",Integer(istr_mant.argumento[2]))
end event

event open;
x				= 0
y				= 0

This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

//pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)


istr_Mant.Argumento[1]	=	'0' // Argumento que almacena Cod. Productor
istr_Mant.Argumento[2]	=	'0' // Argumento que almacena Cod. Variedad
istr_Mant.Argumento[3]	=	''  // Argumento que almacena Cod. Embalaje
istr_Mant.Argumento[4]	=	''  // Argumento que almacena Nombre Nave
istr_Mant.Argumento[5]	=	'0' // Argumento que almacena Cod. Nave
istr_Mant.Argumento[6]	=	''  // Argumento que almacena Nombre Puerto
istr_Mant.Argumento[7]	=	'0' // Argumento que almacena Cod. Puerto
istr_mant.argumento[8]  = 	''	 // Argumento que almacena Nombre Recibidor
istr_mant.argumento[9]  = 	'0' // Argumento que almacena Codigo Recibidor	
istr_mant.argumento[10] = 	''	 // Argumento que almacena Nombre Etiqueta
istr_mant.argumento[11] = 	'0' // Argumento que almacena Codigo Etiqueta
istr_mant.argumento[12] = 	'' // Argumento que almacena nombre Etiqueta







end event

event ue_borra_detalle;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_1.GetItemStatus(il_Fila,0,Primary!)	=	NewModified!	THEN
	IF	dw_1.GetItemStatus(il_Fila,0,Primary!)	=	NewModified!	THEN	
		IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
			IF dw_1.DeleteRow(0) = 1 THEN
				ib_borrar = False
				w_main.SetMicroHelp("Borrando Registro...")
				SetPointer(Arrow!)
			ELSE
				ib_borrar = False
				MessageBox(This.Title,"No se puede borrar actual registro.")
			END IF
		
		 IF dw_1.RowCount() = 0 THEN
				pb_eliminar.Enabled = False
			ELSE
				il_fila = dw_1.GetRow()
			END IF
		END IF
	ELSE
		MessageBox("Atención","No se borrarán registros ya almacenados")
	END IF
ELSE
	MessageBox("Atención","No se borrarán registros ya almacenados")
END If
end event

event ue_imprimir;SetPointer(Arrow!)
Integer	li_fila, li_planta,li_Agronomo, li_Tipo, li_zona
Date		ld_FechaEmbaini, ld_FechaEmbafin


SetPointer(HourGlass!)

istr_info.titulo	= 'INFORME CARGA PLANILLAS EXCEL EN DESTINO: U.K.'

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_ctlcalplanilla_destino_completo"


vinf.dw_1.SetTransObject(sqlca)

li_fila = vinf.dw_1.Retrieve(dw_2.Object.clie_codigo[1],&
									  dw_2.Object.cpde_numero[1],&
									  dw_2.Object.cpde_fecins[1])


IF li_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF li_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)

end event

event ue_antesguardar;Long		ll_Numero, ll_fila = 1
Integer  li_Planta, li_Cliente, li_Secuen, li_cont, li_Causal, li_cont1
String	ls_Mensaje, ls_colu[]

	
IF Isnull(dw_2.Object.puer_codigo[il_fila]) OR dw_2.Object.puer_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Puerto"
	ls_colu[li_cont]	= "puer_codigo"
END IF	

IF Isnull(dw_2.Object.reci_codigo[il_fila]) OR dw_2.Object.reci_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Recibidor"
	ls_colu[li_cont]	= "reci_codigo"
END IF	

IF Isnull(dw_2.Object.nave_codigo[il_fila]) OR dw_2.Object.nave_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo Nave"
	ls_colu[li_cont]	= "nave_codigo"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de " + ls_mensaje + ".", StopSign!, Ok!)
	dw_2.SetColumn(ls_colu[1])
	dw_2.SetFocus()
	Message.DoubleParm = -1
	RETURN
END IF


FOR ll_Fila = 1 TO dw_1.RowCount()
	IF Isnull(dw_1.Object.emba_codigo[ll_Fila]) OR dw_1.Object.emba_codigo[ll_Fila] = '' THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Embalaje"
		ls_colu[li_cont]	= "emba_codigo"
	END IF
	
	IF Isnull(dw_1.Object.vari_codigo[ll_Fila]) OR dw_1.Object.vari_codigo[ll_Fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Variedad"
		ls_colu[li_cont]	= "vari_codigo"
	END IF
	
	IF Isnull(dw_1.Object.etiq_codigo[ll_Fila]) OR dw_1.Object.etiq_codigo[ll_Fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Etiqueta"
		ls_colu[li_cont]	= "etiq_codigo"
	END IF	
NEXT

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF

end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_planilla_uk
integer x = 32
integer y = 756
integer width = 3200
integer height = 988
integer taborder = 80
string title = "Detalle Inspección Destino"
string dataobject = "dw_mues_ctlcalplanilla_destino_det"
boolean vscrollbar = false
boolean hsplitscroll = true
end type

event type long dw_1::ue_seteafila(unsignedlong wparam, long lparam);il_fila	= This.GetRow()

RETURN 0
end event

event dw_1::rowfocuschanged;ib_datos_ok = True



end event

event dw_1::losefocus;AcceptText()
end event

event dw_1::itemchanged;String	ls_Nula, ls_Columna
SetNull(ls_Nula)

ls_Columna	= dwo.name


CHOOSE CASE ls_Columna
		
	CASE "vari_codigo"		
		iuo_Variedades		=	CREATE	uo_Variedades
		
		IF iuo_Variedades.Existe(11,Integer(Data), True, Sqlca) = False THEN
			dw_1.Object.vari_codigo[Row]= Integer(ls_Nula)
			RETURN 1
		ELSE
			dw_1.Object.vari_codigo[Row]	= iuo_Variedades.Variedad
			dw_1.Object.vari_nombre[Row]	= iuo_Variedades.NombreVariedad	
		END IF							
		Destroy iuo_Recibidores
		
   CASE "etiq_codigo"
		
		iuo_Etiquetas		=	CREATE	uo_Etiquetas
		
		IF iuo_Etiquetas.Existe(Integer(Data), True, Sqlca) = False THEN
			dw_1.Object.etiq_codigo[Row]= Integer(ls_Nula)
			RETURN 1
		ELSE
			dw_1.Object.etiq_codigo[Row]	= iuo_Etiquetas.Codigo
			dw_1.Object.etiq_nombre[Row]	= iuo_Etiquetas.Nombre	
		END IF							
		Destroy iuo_Etiquetas
		
	CASE "emba_codigo"	
		
		iuo_Embalajes		=	CREATE	uo_Embalajesprod
		
		IF iuo_Embalajes.Existe(81,Data, True, Sqlca) = False THEN
			dw_1.Object.emba_codigo[Row]= ls_Nula
			RETURN 1
		ELSE
			dw_1.Object.emba_codigo[Row]	= iuo_Embalajes.Codigo
			dw_1.Object.emba_nombre[Row]	= iuo_Embalajes.Nombre	
		END IF							
		Destroy iuo_Embalajes
	
END CHOOSE


end event

event dw_1::getfocus;//return 0
end event

event dw_1::doubleclicked;//
end event

event dw_1::clicked;String ls_Columna
IF Row > 0 THEN
	ls_Columna = dwo.Name
	CHOOSE CASE ls_Columna
			
		CASE "buscavariedad"
			buscavariedad(row)
			
		CASE "buscaembalaje"
			BuscaEmbalaje(row)
			
		CASE "buscaetiqueta"	
			BuscaEtiqueta(row)
	
	END CHOOSE
END IF
end event

event dw_1::itemerror;call super::itemerror;Return 1 
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_planilla_uk
integer x = 96
integer y = 36
integer width = 2821
integer height = 680
string dataobject = "dw_mues_ctlcalplanilla_destino"
end type

event dw_2::itemchanged;String	ls_Columna, ls_Nula

SetNull(ls_Nula)

ls_Columna	=	dwo.Name

dw_2.AcceptText()

CHOOSE CASE ls_Columna	
		
	CASE "reci_codigo"		
		iuo_Recibidores		=	CREATE	uo_Recibidores
		
		IF iuo_Recibidores.Existe(Integer(Data), True, Sqlca) = False THEN
			dw_2.Object.reci_codigo[1]= Integer(ls_Nula)
			RETURN 1
		ELSE
			dw_2.Object.reci_codigo[1]	= iuo_Recibidores.Codigo
			dw_2.Object.reci_nombre[1]	= iuo_Recibidores.Nombre	
		END IF							
		Destroy iuo_Recibidores
		
	CASE "puer_codigo"
		iuo_Puertos		=	CREATE	uo_Puertos

		IF iuo_Puertos.Existe(Integer(Data), True, Sqlca) = False THEN
			dw_2.Object.puer_codigo[1]= Integer(ls_Nula)
			RETURN 1
		ELSE
			dw_2.Object.puer_codigo[1]	= iuo_Puertos.Codigo
			dw_2.Object.puer_nombre[1]	= iuo_Puertos.Nombre	
		END IF
		
		Destroy iuo_Puertos
		
	CASE "nave_codigo"
		
		iuo_Naves	=	CREATE	uo_Naves

		IF iuo_Naves.Existe(Integer(Data),dw_2.Object.nave_tipotr[1], True, Sqlca) = False THEN
			dw_2.Object.nave_codigo[1]= Integer(ls_Nula)
			RETURN 1
		ELSE
			dw_2.Object.nave_codigo[1]	= iuo_Naves.Codigo
			dw_2.Object.nave_nombre[1]	= iuo_Naves.Nombre	
		END IF		
		Destroy iuo_Naves
		
END CHOOSE
end event

event dw_2::clicked;CHOOSE CASE dwo.Name
	CASE "buscapuertos"
		BuscaPuerto()
		
	CASE "buscarecibidor"
		BuscaRecibidor(81)
		
	CASE "buscanave"	
			BuscaNave()

END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_planilla_uk
boolean visible = false
integer x = 3328
integer y = 396
end type

event pb_nuevo::clicked;call super::clicked;dw_1.Reset()
dw_2.Reset()
pb_Grabar.Enabled = False
pb_Imprimir.Enabled = False
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_planilla_uk
boolean visible = false
integer x = 3328
integer y = 576
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_planilla_uk
integer x = 3328
integer y = 764
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_planilla_uk
integer x = 3328
integer y = 936
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_planilla_uk
integer x = 3328
integer y = 1116
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_planilla_uk
boolean visible = false
integer x = 3342
integer y = 1408
integer taborder = 90
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_planilla_uk
boolean visible = false
integer x = 3333
integer y = 1580
integer taborder = 100
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_planilla_uk
integer x = 3328
integer y = 216
end type

