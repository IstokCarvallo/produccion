$PBExportHeader$w_crea_cajas_bultos.srw
$PBExportComments$Emisión Solicitud de Inspección.
forward
global type w_crea_cajas_bultos from w_para_informes
end type
type st_1 from statictext within w_crea_cajas_bultos
end type
type dw_cliente from datawindow within w_crea_cajas_bultos
end type
type st_2 from statictext within w_crea_cajas_bultos
end type
type dw_plantadesp from datawindow within w_crea_cajas_bultos
end type
type em_numero from editmask within w_crea_cajas_bultos
end type
type st_3 from statictext within w_crea_cajas_bultos
end type
type st_5 from statictext within w_crea_cajas_bultos
end type
type dw_16 from datawindow within w_crea_cajas_bultos
end type
type dw_1 from datawindow within w_crea_cajas_bultos
end type
type dw_spro_cajasprod from datawindow within w_crea_cajas_bultos
end type
type st_4 from statictext within w_crea_cajas_bultos
end type
type dw_mercado from datawindow within w_crea_cajas_bultos
end type
type dw_2 from datawindow within w_crea_cajas_bultos
end type
type dw_3 from datawindow within w_crea_cajas_bultos
end type
type st_6 from statictext within w_crea_cajas_bultos
end type
type em_camara from editmask within w_crea_cajas_bultos
end type
type ddlb_sdp from dropdownlistbox within w_crea_cajas_bultos
end type
end forward

global type w_crea_cajas_bultos from w_para_informes
integer width = 4274
integer height = 2552
string title = "IMPRESION DE COMPACTOS"
boolean minbox = false
event ue_validapassword ( )
st_1 st_1
dw_cliente dw_cliente
st_2 st_2
dw_plantadesp dw_plantadesp
em_numero em_numero
st_3 st_3
st_5 st_5
dw_16 dw_16
dw_1 dw_1
dw_spro_cajasprod dw_spro_cajasprod
st_4 st_4
dw_mercado dw_mercado
dw_2 dw_2
dw_3 dw_3
st_6 st_6
em_camara em_camara
ddlb_sdp ddlb_sdp
end type
global w_crea_cajas_bultos w_crea_cajas_bultos

type variables
Str_mant				istr_mant
DataWindowChild	dwc_plantas, idwc_mercado, idwc_Productor
Integer				ii_planta, ii_cliente, il_cont, ii_sdp
Long					il_NroPallet, il_NroCaja, il_secuencia
String					is_Computador

uo_lotescorrelequipo_gr	iuo_correl
uo_voicecode				iuo_voicecode
uo_QR						iuo_QR

end variables

forward prototypes
public function boolean wf_actualiza_db ()
public function boolean buscanuevocorrelativo (integer ai_bodega, integer ai_cliente)
public function boolean analisa_pallet (integer al_pallet)
public function boolean existe_pallet (long al_pallet)
end prototypes

event ue_validapassword();Str_mant					lstr_mant

lstr_mant.Argumento[1]	=	"Produccion"
lstr_mant.Argumento[2]	=	gs_Password+''+'contraparte'

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

public function boolean wf_actualiza_db ();return True
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

//dw_1.Object.capr_numero[1]	=	il_NroCaja

RETURN True
end function

public function boolean analisa_pallet (integer al_pallet);Integer	li_cliente, li_planta

li_cliente = Integer(istr_mant.argumento[1])
li_planta  = Integer(istr_mant.argumento[2])

SELECT max(pafr_secuen)
	INTO	:il_secuencia
	FROM dbo.palletfruta
	WHERE paen_numero = :al_pallet
	AND	clie_codigo = :li_cliente
	AND	plde_codigo = :li_planta;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla palletfruta")
	Return False
END IF	

IF il_secuencia = 0 OR isnull(il_secuencia) THEN
	MessageBox('Atención', 'Pallet NO Existe.')
	Return False	
END IF

IF il_secuencia > 999 THEN
	SELECT count(*)
		INTO	:il_cont
		FROM dbo.spro_cajasprod
		WHERE capr_numero = :il_secuencia
		AND	clie_codigo = :li_cliente
		AND	plde_codigo = :li_planta;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_cajasprod")
		Return False
	END IF	
END IF	

Return True
end function

public function boolean existe_pallet (long al_pallet);Integer	li_cliente, li_planta

li_cliente = Integer(istr_mant.argumento[1])
li_planta  = Integer(istr_mant.argumento[2])

SELECT count(*)
	INTO	:il_cont
	FROM dbo.palletencab
	WHERE paen_numero = :al_pallet
	AND	clie_codigo = :li_cliente
	AND	plde_codigo = :li_planta
	AND	paen_estado = 1;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla PalletEncab")
	Return False
END IF	

IF il_cont = 0 OR isnull(il_cont) THEN
	MessageBox('Atención', 'Pallet NO Existe.')
	Return False	
END IF

Return True
end function

on w_crea_cajas_bultos.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_2=create st_2
this.dw_plantadesp=create dw_plantadesp
this.em_numero=create em_numero
this.st_3=create st_3
this.st_5=create st_5
this.dw_16=create dw_16
this.dw_1=create dw_1
this.dw_spro_cajasprod=create dw_spro_cajasprod
this.st_4=create st_4
this.dw_mercado=create dw_mercado
this.dw_2=create dw_2
this.dw_3=create dw_3
this.st_6=create st_6
this.em_camara=create em_camara
this.ddlb_sdp=create ddlb_sdp
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.dw_plantadesp
this.Control[iCurrent+5]=this.em_numero
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.st_5
this.Control[iCurrent+8]=this.dw_16
this.Control[iCurrent+9]=this.dw_1
this.Control[iCurrent+10]=this.dw_spro_cajasprod
this.Control[iCurrent+11]=this.st_4
this.Control[iCurrent+12]=this.dw_mercado
this.Control[iCurrent+13]=this.dw_2
this.Control[iCurrent+14]=this.dw_3
this.Control[iCurrent+15]=this.st_6
this.Control[iCurrent+16]=this.em_camara
this.Control[iCurrent+17]=this.ddlb_sdp
end on

on w_crea_cajas_bultos.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_2)
destroy(this.dw_plantadesp)
destroy(this.em_numero)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.dw_16)
destroy(this.dw_1)
destroy(this.dw_spro_cajasprod)
destroy(this.st_4)
destroy(this.dw_mercado)
destroy(this.dw_2)
destroy(this.dw_3)
destroy(this.st_6)
destroy(this.em_camara)
destroy(this.ddlb_sdp)
end on

event open;call super::open;dw_cliente.SetTransObject(Sqlca)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

IF gi_CodExport <> gi_cliebase THEN
	em_camara.Enabled = True
	em_camara.Text		= ''
	ddlb_sdp.Enabled  = True
ELSE	
	em_camara.Enabled = False
	ddlb_sdp.Enabled  = False
END IF

dw_1.GetChild("prod_codigo", idwc_Productor)
idwc_Productor.SetTransObject(Sqlca)
idwc_Productor.Retrieve(-1)			

dw_plantadesp.GetChild("plde_codigo", dwc_plantas)
dwc_plantas.SetTransObject(Sqlca)
dwc_plantas.Retrieve(1)			

dw_mercado.GetChild("merc_codigo", idwc_mercado)
idwc_mercado.SetTransObject(SQLCA)
idwc_mercado.Retrieve()
dw_mercado.InsertRow(0)

dw_plantadesp.SetTransObject(Sqlca)
dw_plantadesp.InsertRow(0)
dw_plantadesp.SetItem(1,"plde_codigo", gi_CodPlanta)

istr_mant.Argumento[1]	=	String(gi_CodExport)
istr_mant.Argumento[2]	=	String(gi_CodPlanta)

dw_16.SetTransObject(Sqlca)
dw_1.SetTransObject(Sqlca)
dw_2.SetTransObject(Sqlca)
dw_3.SetTransObject(Sqlca)

iuo_QR	=	Create uo_QR

RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", "ComputerName", RegString!, is_Computador)
end event

event resize;call super::resize;dw_1.Resize(This.WorkSpaceWidth() - 860, This.WorkSpaceHeight() - dw_1.y - 75)

dw_1.x					= st_5.x
end event

type pb_excel from w_para_informes`pb_excel within w_crea_cajas_bultos
integer x = 3653
integer y = 696
integer width = 306
end type

type st_computador from w_para_informes`st_computador within w_crea_cajas_bultos
integer x = 2587
integer y = 152
end type

type st_usuario from w_para_informes`st_usuario within w_crea_cajas_bultos
integer x = 2587
integer y = 80
end type

type st_temporada from w_para_informes`st_temporada within w_crea_cajas_bultos
integer x = 2587
integer y = 8
end type

type p_logo from w_para_informes`p_logo within w_crea_cajas_bultos
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_crea_cajas_bultos
integer width = 3269
string text = "Generación de Cajas e Impresión de Compactos"
end type

type pb_acepta from w_para_informes`pb_acepta within w_crea_cajas_bultos
integer x = 3657
integer y = 1052
integer taborder = 50
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;Long	ll_pallet, ll_caja, ll_FilaPallet, ll_NroPallet, ll_Registros, ll_FilaCajas, ll_Fila, ll_cont,&
		ll_Fila1, ll_final,ll_inicial,ll_filadet, ll_productor, ll_secuencia, ll_prodrot, ll_filnew, ll_ncaja, ll_existe,&
		ll_fill, gl_packing, ll_nrocaja, ll_inicio, ll_numerocaja
Integer	li_planta, li_cliente, li_especie, li_variedad, li_condicion, li_etiqueta, li_mercado, li_secuencia, &
		li_existe, li_existe2, li_resultado, li_formato, li_categoria, li_carotulado
String	ls_dw, ls_camara, ls_embalaje, ls_calibre, ls_fecha, ls_gtin_numero, ls_formato, ls_CSG, ls_Codigo, ls_QR, ls_Ruta
Boolean	lb_autocommit

li_planta = Integer(istr_mant.argumento[2])

If em_numero.Text = '' Then
	MessageBox( "Atención", "Falta Número de Pallet.",	StopSign!, OK!)
	em_numero.SetFocus()
	Return 1
End If	
				
dw_16.SetTransObject(sqlca)
dw_spro_cajasprod.SetTransObject(sqlca)

li_mercado = dw_mercado.Object.merc_codigo[1]

If IsNull(li_mercado) OR li_mercado = 0 Then 
	MessageBox( "No Existe información", "Falta Ingreso de Mercado.", StopSign!, OK!)
	Return 1						
End If

If li_mercado = -1 Then Return
iuo_correl					=	Create	uo_lotescorrelequipo_gr
iuo_correl.ExisteCorrel(Integer(istr_mant.argumento[2]), 99, is_Computador, FALSE, sqlca)

SELECT foad_canocx
INTO :li_formato
FROM dbo.spro_formatosadhesivos
WHERE foad_nofodw = :iuo_correl.loco_dwcomp;

If iuo_correl.loco_dwcomp = '' Then
	MessageBox( "Atención", "Falta Mantención a Tabla Correlativos de Compactos.", StopSign!, OK!)
	Return 1
End If	

dw_16.DataObject = iuo_correl.loco_dwcomp   // ls_dw
dw_16.SetTransObject(Sqlca)

li_cliente	= dw_cliente.Object.clie_codigo[1]
li_planta 	= dw_plantadesp.Object.plde_codigo[1]
ll_pallet 	= Long(em_numero.Text)

ll_cont = dw_1.Retrieve(Integer(istr_mant.argumento[1]),ll_pallet,Integer(istr_mant.argumento[2]), ll_caja)

If IsNull(dw_1.Object.emba_nrogs1[1])  OR dw_1.Object.emba_nrogs1[1] = '' Then
	li_resultado = MessageBox("Atención", 'Código GS1 NO Existe en Tabla Respectiva, Desea Continuar', Exclamation!, OKCancel!, 2)
	ls_gtin_numero = '00000000000000'	
	If li_resultado <> 1 Then Return 1
Else
	ls_gtin_numero =  dw_1.Object.emba_nrogs1[1]
End If

If ll_existe = 0 Then
	ll_inicial = iuo_correl.il_correcompa
	ll_ncaja = iuo_correl.il_correcompa
End If	

	If ll_cont > 0 Then
		FOR ll_filadet = 1 TO dw_1.RowCount()
			FOR 	ll_fila = 1 TO dw_1.Object.pafr_ccajas[ll_filadet]
									
				li_cliente		= dw_1.Object.clie_codigo[ll_filadet]
				ll_pallet	 	= dw_1.Object.paen_numero[ll_filadet]
				li_especie	= dw_1.Object.espe_codigo[ll_filadet]
				li_variedad 	= dw_1.Object.vari_codigo[ll_filadet]
				ls_embalaje	= dw_1.Object.emba_codigo[ll_filadet]
				ll_productor = dw_1.Object.prod_codigo[ll_filadet]
				li_condicion	= dw_1.Object.cond_codigo[ll_filadet]
				li_etiqueta 	= dw_1.Object.etiq_codigo[ll_filadet]
				li_planta		= dw_1.Object.plde_codigo[ll_filadet]
				ls_calibre  	= dw_1.Object.pafr_calrot[ll_filadet]
				ll_secuencia	= dw_1.Object.pafr_secuen[ll_filadet]
				ll_prodrot	= dw_1.Object.pafr_prdrot[ll_filadet]
				li_categoria	= dw_1.Object.cate_codigo[ll_filadet]
				li_carotulado= dw_1.Object.pafr_catrot[ll_filadet]
			
					ll_filnew = dw_spro_cajasprod.InsertRow(0)
					
					dw_spro_cajasprod.Object.clie_codigo[ll_filnew] = li_cliente  
					dw_spro_cajasprod.Object.plde_codigo[ll_filnew] = li_planta  
					dw_spro_cajasprod.Object.capr_numero[ll_filnew] = ll_ncaja   
					dw_spro_cajasprod.Object.espe_codigo[ll_filnew] = li_especie  
					dw_spro_cajasprod.Object.vari_codigo[ll_filnew] = li_variedad  
					dw_spro_cajasprod.Object.prod_codigo[ll_filnew] = ll_productor  
					dw_spro_cajasprod.Object.prod_predio[ll_filnew] = dw_1.Object.pafr_huert4[ll_filadet]  
					dw_spro_cajasprod.Object.prod_huerto[ll_filnew] = dw_1.Object.pafr_huert4[ll_filadet]  
					dw_spro_cajasprod.Object.prod_cuarte[ll_filnew] = dw_1.Object.pafr_cuart4[ll_filadet]  
					dw_spro_cajasprod.Object.emba_codigo[ll_filnew] = ls_embalaje  
					dw_spro_cajasprod.Object.etiq_codigo[ll_filnew] = li_etiqueta 
					If NOT IsNull(dw_1.Object.pafr_fecrot[ll_filadet]) Then 
						dw_spro_cajasprod.Object.capr_fecemb[ll_filnew] = dw_1.Object.pafr_fecrot[ll_filadet]   
					Else	
						dw_spro_cajasprod.Object.capr_fecemb[ll_filnew] = dw_1.Object.pafr_fecemb[ll_filadet]   
					End If	
					dw_spro_cajasprod.Object.capr_calibr[ll_filnew] = ls_calibre  
					dw_spro_cajasprod.Object.capr_numpal[ll_filnew] = ll_pallet 
					dw_spro_cajasprod.Object.capr_numtra[ll_filnew] = ll_pallet
					dw_spro_cajasprod.Object.capr_estado[ll_filnew] = 1  
					dw_spro_cajasprod.Object.capr_varrot[ll_filnew] = dw_1.Object.pafr_varrot[ll_filadet]  
					dw_spro_cajasprod.Object.cate_codigo[ll_filnew] = dw_1.Object.cate_codigo[ll_filadet]  
					dw_spro_cajasprod.Object.capr_cespak[ll_filnew] = dw_1.Object.pafr_rotpak[ll_filadet] 
					//dw_spro_cajasprod.Object.capr_docrel[ll_filnew] = dw_1.Object.pafr_docrel[ll_filadet] 
					dw_spro_cajasprod.Object.capr_hordig[ll_filnew] = Now()  
					dw_spro_cajasprod.Object.capr_fecdig[ll_filnew] = Today()  
					//dw_spro_cajasprod.Object.capr_nrlote[ll_filnew] = dw_1.Object.pafr_nrlote[ll_filadet]
					dw_spro_cajasprod.Object.capr_lineas[ll_filnew] = iuo_correl.loco_comlin  
					dw_spro_cajasprod.Object.capr_pcline[ll_filnew] = is_computador
					dw_spro_cajasprod.Object.capr_prdrot[ll_filnew] = ll_prodrot
					dw_spro_cajasprod.Object.cama_nombre[ll_filnew] = dw_1.Object.cama_nombre[ll_filadet] 
					dw_spro_cajasprod.Object.capr_catrot[ll_filnew] = dw_1.Object.pafr_catrot[ll_filadet] 
					
					
					dw_3.Object.clie_codigo[ll_filnew] 		= dw_1.Object.clie_codigo[ll_filadet]     
					dw_3.Object.paen_numero[ll_filnew] 	= dw_1.Object.paen_numero[ll_filadet]  
					dw_3.Object.espe_codigo[ll_filnew] 		= dw_1.Object.espe_codigo[ll_filadet]  
					dw_3.Object.vari_codigo[ll_filnew] 		= dw_1.Object.vari_codigo[ll_filadet]  
					dw_3.Object.emba_codigo[ll_filnew] 		= dw_1.Object.emba_codigo[ll_filadet]  
					dw_3.Object.prod_codigo[ll_filnew] 		= dw_1.Object.prod_codigo[ll_filadet]  
					dw_3.Object.cond_codigo[ll_filnew] 		= dw_1.Object.cond_codigo[ll_filadet]  
					dw_3.Object.etiq_codigo[ll_filnew] 		= dw_1.Object.etiq_codigo[ll_filadet]  
					dw_3.Object.plde_codigo[ll_filnew] 		= dw_1.Object.plde_codigo[ll_filadet]  
					dw_3.Object.pafr_calibr[ll_filnew]	 		= dw_1.Object.pafr_calibr[ll_filadet]    
					dw_3.Object.pafr_secuen[ll_filnew] 		= ll_ncaja   
					dw_3.Object.pafr_ccajas[ll_filnew] 		= 1    
					dw_3.Object.pafr_nrlote[ll_filnew] 		= dw_1.Object.pafr_nrlote[ll_filadet]     
					dw_3.Object.pafr_copack[ll_filnew] 		= dw_1.Object.pafr_copack[ll_filadet]     
					dw_3.Object.pafr_varrot[ll_filnew] 		= dw_1.Object.pafr_varrot[ll_filadet]     
					dw_3.Object.pafr_prdrot[ll_filnew] 		= dw_1.Object.pafr_prdrot[ll_filadet]     
					dw_3.Object.pafr_calrot[ll_filnew] 		= dw_1.Object.pafr_calrot[ll_filadet]     
					dw_3.Object.pafr_huert1[ll_filnew] 		= dw_1.Object.pafr_huert1[ll_filadet]     
					dw_3.Object.pafr_cuart1[ll_filnew] 		= dw_1.Object.pafr_cuart1[ll_filadet]     
					dw_3.Object.pafr_fecemb[ll_filnew] 		= dw_1.Object.pafr_fecemb[ll_filadet]     
					dw_3.Object.pafr_fecing[ll_filnew] 		= dw_1.Object.pafr_fecing[ll_filadet]     
					dw_3.Object.pafr_fecdes[ll_filnew] 		= dw_1.Object.pafr_fecdes[ll_filadet]     
					dw_3.Object.pafr_cjssal[ll_filnew] 		= 1     
					dw_3.Object.pafr_huert4[ll_filnew] 		= dw_1.Object.pafr_huert4[ll_filadet]     
					dw_3.Object.pafr_cuart4[ll_filnew] 		= dw_1.Object.pafr_cuart4[ll_filadet]     
					dw_3.Object.pafr_rotpak[ll_filnew] 		= dw_1.Object.pafr_rotpak[ll_filadet]     
					dw_3.Object.pafr_huert2[ll_filnew] 		= dw_1.Object.pafr_huert2[ll_filadet]     
					dw_3.Object.pafr_huert3[ll_filnew] 		= dw_1.Object.pafr_huert3[ll_filadet]     
					dw_3.Object.pafr_cuart2[ll_filnew] 		= dw_1.Object.pafr_cuart2[ll_filadet]     
					dw_3.Object.pafr_cuart3[ll_filnew] 		= dw_1.Object.pafr_cuart3[ll_filadet]     
					dw_3.Object.pafr_barra1[ll_filnew] 		= dw_1.Object.pafr_barra1[ll_filadet]     
					dw_3.Object.pafr_barra2[ll_filnew] 		= dw_1.Object.pafr_barra2[ll_filadet]     
					dw_3.Object.pafr_barra3[ll_filnew] 		= dw_1.Object.pafr_barra3[ll_filadet]     
					dw_3.Object.pafr_barra4[ll_filnew] 		= dw_1.Object.pafr_barra4[ll_filadet]     
					dw_3.Object.tran_fechat[ll_filnew] 		= dw_1.Object.tran_fechat[ll_filadet]     
					dw_3.Object.pafr_docrel[ll_filnew] 		= dw_1.Object.pafr_docrel[ll_filadet]     
					dw_3.Object.pafr_tipdoc[ll_filnew] 		= dw_1.Object.pafr_tipdoc[ll_filadet]     
					dw_3.Object.pafr_fecrot[ll_filnew] 		= dw_1.Object.pafr_fecrot[ll_filadet]     
					dw_3.Object.pafr_nroori[ll_filnew] 		= dw_1.Object.pafr_nroori[ll_filadet]     
					dw_3.Object.pafr_secori[ll_filnew] 		= dw_1.Object.pafr_secori[ll_filadet]     
					dw_3.Object.pafr_embrea[ll_filnew] 		= dw_1.Object.pafr_embrea[ll_filadet] 
					dw_3.Object.cama_nombre[ll_filnew]		= dw_1.Object.cama_nombre[ll_filadet] 
					dw_3.Object.cate_codigo[ll_filnew] 		= dw_1.Object.cate_codigo[ll_filadet] 
					dw_3.Object.pafr_catrot[ll_filnew] 		= dw_1.Object.pafr_catrot[ll_filadet] 
									
				//End If
				
				lb_autocommit = SQLCA.Autocommit
				SQLCA.Autocommit = FALSE
				
				If dw_spro_cajasprod.Update(True, False) = 1 Then
					Commit;
					If dw_3.Update(True, False) = 1 Then
						Commit;
						If sqlca.SQLCode <> 0 Then
							F_ErrorBaseDatos(sqlca, 'Palletfruta')
							RollBack;
							Return
						Else
							dw_3.ResetUpdate()
						End If	
					Else	
						F_ErrorBaseDatos(sqlca, 'Palletfruta')
						RollBack;
					End If	
					
					If sqlca.SQLCode <> 0 Then
						F_ErrorBaseDatos(sqlca, 'Spro_cajasprod')
						RollBack;
						Return
					Else								
						dw_spro_cajasprod.ResetUpdate()
					End If
				Else
					F_ErrorBaseDatos(sqlca, 'Spro_cajasprod')
					RollBack;
					Return
				End If
				ll_Ncaja ++
			NEXT	
		NEXT
			
		If dw_1.RowCount() > 0 Then
			dw_1.RowsMove(1, dw_1.RowCount(), Primary!, dw_1, 1, Delete!)
		End If
		
		If dw_1.Update(True, False) = 1 Then
			Commit;
				
			If sqlca.SQLCode <> 0 Then
				F_ErrorBaseDatos(sqlca, 'Palletfruta')
				RollBack;
				Return
			Else
														
				dw_1.ResetUpdate()
				
			End If
		Else
			F_ErrorBaseDatos(sqlca, 'Palletfruta')
			RollBack;
			Return
		End If
				
		dw_1.Reset()		
		
		dw_1.Retrieve(Integer(istr_mant.argumento[1]),ll_pallet,Integer(istr_mant.argumento[2]),ll_caja)
	Else
		MessageBox( "NO Existe Pallet", "Pallet NO Tiene Detalle.", StopSign!, OK!)
		dw_1.Reset()
		em_numero.Text = ''
		em_numero.SetFocus()
		Return
	End If	
	
	SELECT Count(*),max(pafr_secuen)
	INTO :ll_existe,:ll_final
	FROM dbo.palletfruta
	WHERE clie_codigo = :li_cliente
	AND	plde_codigo = :li_planta
	AND	paen_numero = :ll_pallet;
	
	SELECT Count(*),min(pafr_secuen)
	INTO :ll_existe,:ll_inicial
	FROM dbo.palletfruta
	WHERE clie_codigo = :li_cliente
	AND	plde_codigo = :li_planta
	AND	paen_numero = :ll_pallet;
	
	dw_1.Retrieve(Integer(istr_mant.argumento[1]),ll_pallet,Integer(istr_mant.argumento[2]),ll_caja)
	
	ll_Fila = 0
	
	If IsNull(dw_1.Object.emba_nrogs1[1])  OR dw_1.Object.emba_nrogs1[1] = '' Then
		ls_gtin_numero = '00000000000000'
	Else
		ls_gtin_numero =  dw_1.Object.emba_nrogs1[1]
	End If
				
	ll_Fila1 = dw_2.Retrieve(li_cliente,li_planta,ll_inicial,ll_final,li_mercado)
	// Compacto Doble
	If li_formato = 2 Then
		FOR ll_fill = 1 TO ll_Fila1 step 2
			
			ll_nrocaja = dw_2.Object.capr_numero[ll_fill]
			gl_packing = dw_2.Object.plde_codigo[ll_fill]
			
			
			ll_inicio	=	Long(dw_2.Object.capr_numero[ll_fill])
						
			If ll_Fill + 1 <= dw_2.RowCount() Then
				ll_final		=	Long(dw_2.Object.capr_numero[ll_fill + 1])
			Else
				ll_final		=	Long(dw_2.Object.capr_numero[ll_fill])
			End If
			
			If Integer(istr_mant.argumento[1]) <> gi_Cliebase Then
				dw_16.Retrieve(li_cliente,li_planta,ll_inicio,ll_final,li_mercado,em_camara.Text,ii_sdp)
			Else
				dw_16.Retrieve(li_cliente,li_planta,ll_inicio,ll_final,li_mercado)
			End If	
			
			dw_16.Object.Ole_1.Object.BarCode	=	Integer(dw_1.Object.clie_codbar[1])
			
			dw_16.Object.Ole_1.Object.BarCode = Integer(dw_1.Object.clie_codbar[1])
			//	sle_1.Text								=	"ImprimiEndo Adhesivos en Formato GS1"
			ls_fecha									=	String(dw_1.Object.pafr_fecemb[1])
			ls_fecha									=	Left(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Right(ls_fecha, 2)
			dw_16.Object.Ole_1.Object.Text 	= 	"01" + ls_gtin_numero + "10" + ls_fecha /*+ "\F" + &
															"21" + String(gl_packing, "0000") +  &
															String(ll_inicio, '00000000')*/
			
			If ll_fill + 1 <= dw_2.RowCount() Then
				
				dw_16.Object.Ole_2.Object.BarCode	=	Integer(dw_1.Object.clie_codbar[1])
			
				ll_nrocaja = dw_2.Object.capr_numero[ll_fill + 1]
				gl_packing = dw_2.Object.plde_codigo[ll_fill + 1]
								
				dw_16.Object.Ole_2.Object.Text 	= 	"01" + ls_gtin_numero + "10" + ls_fecha /*+ "\F" + &
																"21" + String(gl_packing, "0000") + &
																String(ll_final, '00000000')*/
			End If																	
			
			dw_16.AcceptText()
			dw_16.Print()
		NEXT			
	Else
		FOR ll_fill = 1 TO ll_Fila1
			
			ll_nrocaja = dw_2.Object.capr_numero[ll_fill]
			gl_packing = dw_2.Object.plde_codigo[ll_fill]
			
			If Integer(istr_mant.argumento[1]) <> gi_Cliebase Then
				dw_16.Retrieve(li_cliente,li_planta,ll_nrocaja,ll_nrocaja,li_mercado,em_camara.Text,ii_sdp)
			Else
				dw_16.Retrieve(li_cliente,li_planta,ll_nrocaja,ll_nrocaja,li_mercado)
			End If	
			
			dw_16.Object.Ole_1.Object.BarCode	=	Integer(dw_1.Object.clie_codbar[1])
			
			If dw_16.Object.Ole_1.Object.BarCode = 20 Then
//				sle_1.Text								=	"ImprimiEndo Adhesivos en Formato CodeBar128"
				dw_16.Object.Ole_1.Object.Text 	= 	'00' + &
																String(dw_1.Object.zona_codigo[1],'00') + &
																String(dw_1.Object.plde_codigo[1],'0000') + &
																String(ll_nrocaja,'0000000000')
				
			ElseIf dw_16.Object.Ole_1.Object.BarCode = 88 Then
					ls_fecha								=	String(dw_1.Object.pafr_fecemb[1])
					ls_fecha								=	Right(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Left(ls_fecha, 2)
					
//					If IsNull(dw_1.Object.emba_nroint[1]) OR Len(String(dw_1.Object.emba_nroint[1])) <> 14 Then
//						MessageBox("Error de Codigo", "Existe un problema con el codigo GTIN14 para el embalaje utilizado," + &
//																"~r~nFavor de comunicar situación a encargado de la linea")
//						Return
//					End If
					
					If IsNull(dw_1.Object.prpr_prepro[1]) Or dw_1.Object.prpr_prepro[1] = "" Then 
						ls_CSG = "0000000"
					Else
						ls_CSG	= Fill('0', 7 - Len(String(dw_1.Object.prpr_prepro[1]))) + dw_1.Object.prpr_prepro[1]
					End If
					
					ll_productor		=	long(dw_1.Object.prod_codigo[1])
					ls_codigo			=	"01" + ls_gtin_numero + "13" + ls_fecha /*+  String(ll_productor, '00000') + "\F"*/
					ls_codigo			=	ls_codigo	 +	"10" + ls_CSG  //+ "\F"
//					ls_codigo			=	ls_codigo	 +	"21" + String(dw_1.Object.plde_codigo[1], "0000") +	String(ll_nrocaja, '00000000') + "\F"
//					ls_codigo			=	ls_codigo	 +	"21" + String(dw_1.Object.pafr_copack[1], "0000") +	String(ll_nrocaja, '00000000') + "\F"
					
					/*
					Codigo QR
					*/		
					ls_Embalaje = dw_1.Object.emba_codigo[1]
					ls_Calibre	= dw_1.Object.pafr_calibr[1]
					
					
					ls_QR	=  String(dw_1.Object.plde_codigo[1], "0000") +	String(ll_nrocaja, '00000000') + Char(10) 
					ls_QR	+= 'Prod:' + String(ll_Productor, '00000') + Char(10) 
					ls_QR	+= 'Esp:' + String(dw_1.Object.espe_codigo[1], '00') + ' / ' + String(dw_1.Object.vari_codigo[1], '0000')  + Char(10) 
					ls_QR	+= 'Pred:' + ls_CSG + ' / ' + String(dw_1.Object.pafr_cuart4[1],'000' )  + Char(10)
					ls_QR	+= 'Emb:' + ls_Embalaje + ' / ' + ls_Calibre + Char(10)
					ls_QR	+= String(dw_1.Object.pafr_fecemb[1], 'dd/mm/yyyy')
					
					ls_Ruta = iuo_QR.of_genera_qr(ls_QR)
					dw_16.Object.p_qrcode.FileName = ls_Ruta
					
					//Codigo UPC
					dw_16.Object.Ole_UPC.Object.Text 	= 	dw_1.Object.gtin_codupc[1]
					dw_16.Object.Ole_1.Object.Text 		= 	ls_codigo	
			End If
			/*
			Code Pick Voice
			 */
			iuo_voicecode	=	Create uo_voicecode
			iuo_voicecode.voicecode(dw_16	, dw_16.Object.emba_nroint[1],  ls_fecha, dw_16.Object.capr_fecemb[1], iuo_correl.foad_vopico)
			
			dw_16.AcceptText()
			dw_16.Print()	
		NEXT	
	End If	
	
	If ll_Fila1 = -1 Then
		MessageBox( "Error en Base de Datos","Se ha producido un error en Base de Datos : ~n" + sqlca.SQLErrText, StopSign!, OK!)
	ElseIf ll_Fila1 = 0 Then
		MessageBox( "No Existe información", "No Existe información para este informe.", StopSign!, OK!)
	End If

dw_16.Reset()
dw_2.Reset()

pb_salir.PostEvent(Clicked!)		

end event

type pb_salir from w_para_informes`pb_salir within w_crea_cajas_bultos
integer x = 3657
integer y = 1416
integer taborder = 60
end type

type st_1 from statictext within w_crea_cajas_bultos
integer x = 338
integer y = 472
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_crea_cajas_bultos
integer x = 649
integer y = 460
integer width = 1253
integer height = 96
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String		ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1] = data
	dwc_plantas.Retrieve(1)	
	
//	IF istr_mant.argumento[1]  = '590' THEN
//		em_camara.Enabled = True
//	ELSE	
//		em_camara.Enabled = False
//		em_camara.Text = ''
//	END IF	

IF Integer(data) <> gi_Cliebase THEN
	em_camara.Enabled = False
	em_camara.Text		= ''
	ddlb_sdp.Enabled  = False
ELSE	
	em_camara.Enabled = True
	ddlb_sdp.Enabled  = True
END IF

	
ELSE
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_crea_cajas_bultos
integer x = 2107
integer y = 464
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type dw_plantadesp from datawindow within w_crea_cajas_bultos
integer x = 2400
integer y = 484
integer width = 992
integer height = 92
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String		ls_Columna[]

IF ExistePlanta(Integer(istr_mant.Argumento[1]), Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2] = data
ELSE
	RETURN 1
END IF
end event

type em_numero from editmask within w_crea_cajas_bultos
integer x = 2400
integer y = 552
integer width = 489
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Long	ll_cont, ll_pallet

dw_1.reset()

istr_mant.argumento[4]	=	This.Text
ll_pallet					=	Long(This.Text)

IF This.Text <> '' THEN
	IF NOT existe_pallet(ll_pallet) THEN
		This.Text = ''
		This.SetFocus()
		Return
	END IF		
	
//	IF il_cont > 0 THEN
//		MessageBox( "Atención", "Pallet ya es Caja a Caja.",StopSign!, OK!)
//		This.Text = ''
//		This.SetFocus()
//		Return
	END IF	
	
	ll_cont = dw_1.Retrieve(Integer(istr_mant.argumento[1]),ll_pallet,Integer(istr_mant.argumento[2]))
	
	IF ll_cont = 0 THEN
		MessageBox( "No Existe información", "Faltan Datos Para Generar Cajas.",StopSign!, OK!)
		This.Text = ''
		Return
	END IF	
//END IF	
end event

type st_3 from statictext within w_crea_cajas_bultos
integer x = 2107
integer y = 560
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Pallet"
boolean focusrectangle = false
end type

type st_5 from statictext within w_crea_cajas_bultos
integer x = 247
integer y = 440
integer width = 3269
integer height = 336
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_16 from datawindow within w_crea_cajas_bultos
boolean visible = false
integer x = 1211
integer y = 1924
integer width = 581
integer height = 444
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_spro_cajasprod_carozos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_1 from datawindow within w_crea_cajas_bultos
integer x = 242
integer y = 788
integer width = 3269
integer height = 1116
boolean bringtotop = true
boolean titlebar = true
string title = "Detalle Pallet"
string dataobject = "dw_mues_palletfruta_pallet_bultos_compacto"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_spro_cajasprod from datawindow within w_crea_cajas_bultos
boolean visible = false
integer x = 315
integer y = 1960
integer width = 686
integer height = 400
integer taborder = 80
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_spro_cajasprod_compacto"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
end type

type st_4 from statictext within w_crea_cajas_bultos
integer x = 338
integer y = 560
integer width = 283
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Mercado"
boolean focusrectangle = false
end type

type dw_mercado from datawindow within w_crea_cajas_bultos
integer x = 649
integer y = 556
integer width = 1006
integer height = 108
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_mercado"
boolean border = false
boolean livescroll = true
end type

event itemchanged;pb_acepta.Enabled = True
end event

type dw_2 from datawindow within w_crea_cajas_bultos
boolean visible = false
integer x = 1975
integer y = 1948
integer width = 686
integer height = 400
boolean bringtotop = true
string title = "none"
string dataobject = "dw_adhesivo"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within w_crea_cajas_bultos
boolean visible = false
integer x = 3625
integer y = 168
integer width = 480
integer height = 352
integer taborder = 70
boolean bringtotop = true
string title = "Detalle Pallet"
string dataobject = "dw_mues_palletfruta_pallet_bultos_compacto"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_6 from statictext within w_crea_cajas_bultos
boolean visible = false
integer x = 338
integer y = 656
integer width = 283
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
boolean enabled = false
string text = "Cámara"
boolean focusrectangle = false
end type

type em_camara from editmask within w_crea_cajas_bultos
boolean visible = false
integer x = 654
integer y = 656
integer width = 1006
integer height = 84
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
boolean border = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
end type

type ddlb_sdp from dropdownlistbox within w_crea_cajas_bultos
integer x = 2400
integer y = 660
integer width = 480
integer height = 400
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
string item[] = {"SDP","SAG"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF Integer(index) = 1 THEN
	ii_sdp = 0
ELSE
	ii_sdp = 1
END IF	


end event

