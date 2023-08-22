$PBExportHeader$w_imprecion_de_compactos_pallet_bultos.srw
$PBExportComments$Emisión Solicitud de Inspección.
forward
global type w_imprecion_de_compactos_pallet_bultos from w_para_informes
end type
type st_1 from statictext within w_imprecion_de_compactos_pallet_bultos
end type
type dw_cliente from datawindow within w_imprecion_de_compactos_pallet_bultos
end type
type st_2 from statictext within w_imprecion_de_compactos_pallet_bultos
end type
type dw_plantadesp from datawindow within w_imprecion_de_compactos_pallet_bultos
end type
type em_numero from editmask within w_imprecion_de_compactos_pallet_bultos
end type
type st_3 from statictext within w_imprecion_de_compactos_pallet_bultos
end type
type st_5 from statictext within w_imprecion_de_compactos_pallet_bultos
end type
type dw_16 from datawindow within w_imprecion_de_compactos_pallet_bultos
end type
type dw_1 from datawindow within w_imprecion_de_compactos_pallet_bultos
end type
type dw_spro_cajasprod from datawindow within w_imprecion_de_compactos_pallet_bultos
end type
type st_4 from statictext within w_imprecion_de_compactos_pallet_bultos
end type
type dw_mercado from datawindow within w_imprecion_de_compactos_pallet_bultos
end type
type dw_2 from datawindow within w_imprecion_de_compactos_pallet_bultos
end type
end forward

global type w_imprecion_de_compactos_pallet_bultos from w_para_informes
integer width = 2816
integer height = 1424
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
end type
global w_imprecion_de_compactos_pallet_bultos w_imprecion_de_compactos_pallet_bultos

type variables
Str_mant				istr_mant
DataWindowChild	dwc_plantas, idwc_mercado
Integer	ii_planta, ii_cliente
Long		il_NroPallet, il_NroCaja
String	is_Computador


uo_lotescorrelequipo		iuo_correl

end variables

forward prototypes
public function boolean wf_actualiza_db ()
public function boolean buscanuevocorrelativo (integer ai_bodega, integer ai_cliente)
end prototypes

event ue_validapassword();Str_mant		lstr_mant

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

on w_imprecion_de_compactos_pallet_bultos.create
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
end on

on w_imprecion_de_compactos_pallet_bultos.destroy
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
end on

event open;call super::open;dw_cliente.SetTransObject(Sqlca)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

//IF gi_CodExport = 590 THEN
//	em_camara.Enabled = True
//ELSE	
//	em_camara.Enabled = False
//END IF	
//
dw_plantadesp.GetChild("plde_codigo", dwc_plantas)
dwc_plantas.SetTransObject(Sqlca)
dwc_plantas.Retrieve(1)			//Plantas de Despacho

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

RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", &
				"ComputerName", RegString!, is_Computador)

//PostEvent("ue_validapassword")





end event

event resize;//
end event

type st_computador from w_para_informes`st_computador within w_imprecion_de_compactos_pallet_bultos
end type

type st_usuario from w_para_informes`st_usuario within w_imprecion_de_compactos_pallet_bultos
end type

type st_temporada from w_para_informes`st_temporada within w_imprecion_de_compactos_pallet_bultos
end type

type p_logo from w_para_informes`p_logo within w_imprecion_de_compactos_pallet_bultos
end type

type st_titulo from w_para_informes`st_titulo within w_imprecion_de_compactos_pallet_bultos
integer width = 1989
string text = "Impresión de Compactos"
end type

type pb_acepta from w_para_informes`pb_acepta within w_imprecion_de_compactos_pallet_bultos
integer x = 2441
integer y = 432
integer taborder = 50
end type

event pb_acepta::clicked;Long	ll_pallet, ll_caja, ll_FilaPallet, ll_NroPallet, ll_Registros, ll_FilaCajas, ll_Fila, ll_cont,&
		ll_Fila1, ll_final,ll_inicial,ll_filadet, ll_productor, ll_secuencia, ll_prodrot, ll_filnew, ll_ncaja, ll_existe,&
		ll_fill, gl_packing, ll_nrocaja, ll_inicio
Integer	li_planta, li_cliente, li_especie, li_variedad, li_condicion, li_etiqueta, li_mercado
String	ls_dw, ls_camara, ls_embalaje, ls_calibre, ls_fecha
Boolean	lb_autocommit

il_NroCaja = 0

li_planta = Integer(istr_mant.argumento[2])

RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", &
				"ComputerName", RegString!,is_Computador)
				
dw_16.SetTransObject(sqlca)
dw_spro_cajasprod.SetTransObject(sqlca)

li_mercado = dw_mercado.Object.merc_codigo[1]

IF isnull(li_mercado) OR li_mercado = 0 THEN 
	MessageBox( "No Existe información", &
						"Falta Ingreso de Mercado.", &
						StopSign!, OK!)
	Return 1						
END IF						

IF li_mercado = -1 THEN
	Return
END IF

iuo_correl					=	Create	uo_lotescorrelequipo

SELECT loco_dwcomp
INTO	:ls_dw
FROM DBA.spro_correlcompequipo
WHERE plde_codigo = :li_planta
AND	equi_nombre = :is_Computador;

dw_16.DataObject = ls_dw

dw_16.SetTransObject(Sqlca)

li_cliente = dw_cliente.Object.clie_codigo[1]
li_planta  = dw_plantadesp.Object.plde_codigo[1]
ll_pallet  = Long(em_numero.Text)

SELECT Count(),max(capr_numero)
INTO :ll_existe,:ll_final
FROM dba.spro_cajasprod
WHERE clie_codigo = :li_cliente
AND	plde_codigo = :li_planta
AND	capr_numpal = :ll_pallet;

SELECT Count(),min(capr_numero)
INTO :ll_existe,:ll_inicial
FROM dba.spro_cajasprod
WHERE clie_codigo = :li_cliente
AND	plde_codigo = :li_planta
AND	capr_numpal = :ll_pallet;

ll_cont = dw_1.Retrieve(Integer(istr_mant.argumento[1]),ll_pallet,Integer(istr_mant.argumento[2]),ll_caja)

ii_planta = li_planta

IF ll_existe = 0 THEN
	IF BuscaNuevoCorrelativo(li_planta,li_cliente) = True THEN
	END IF
	
	ll_inicial = iuo_correl.il_correcompa
	
	ll_ncaja = iuo_correl.il_correcompa
END IF	

IF ll_existe = 0 THEN
	IF ll_cont > 0 THEN
		FOR ll_filadet = 1 TO dw_1.RowCount()
			FOR 	ll_fila = 1 TO dw_1.Object.pafr_ccajas[ll_filadet]
			
				li_cliente 	 = dw_1.Object.clie_codigo[ll_filadet]
				ll_pallet	 = dw_1.Object.paen_numero[ll_filadet]
				li_especie	 = dw_1.Object.espe_codigo[ll_filadet]
				li_variedad  = dw_1.Object.vari_codigo[ll_filadet]
				ls_embalaje  = dw_1.Object.emba_codigo[ll_filadet]
				ll_productor = dw_1.Object.prod_codigo[ll_filadet]
				li_condicion = dw_1.Object.cond_codigo[ll_filadet]
				li_etiqueta  = dw_1.Object.etiq_codigo[ll_filadet]
				li_planta	 = dw_1.Object.plde_codigo[ll_filadet]
				ls_calibre   = dw_1.Object.pafr_calibr[ll_filadet]
				ll_secuencia = dw_1.Object.pafr_secuen[ll_filadet]
				ll_prodrot 	 = dw_1.Object.pafr_prdrot[ll_filadet]
				
				ll_filnew = dw_spro_cajasprod.InsertRow(0)
				
				dw_spro_cajasprod.Object.clie_codigo[ll_filnew] = li_cliente  
				dw_spro_cajasprod.Object.plde_codigo[ll_filnew] = li_planta  
				dw_spro_cajasprod.Object.capr_numero[ll_filnew] = ll_ncaja   
				dw_spro_cajasprod.Object.espe_codigo[ll_filnew] = li_especie  
				dw_spro_cajasprod.Object.vari_codigo[ll_filnew] = li_variedad  
				dw_spro_cajasprod.Object.prod_codigo[ll_filnew] = ll_productor  
				dw_spro_cajasprod.Object.prod_predio[ll_filnew] = dw_1.Object.pafr_huert1[ll_filadet]  
				dw_spro_cajasprod.Object.prod_huerto[ll_filnew] = dw_1.Object.pafr_huert1[ll_filadet]  
				dw_spro_cajasprod.Object.prod_cuarte[ll_filnew] = dw_1.Object.pafr_cuart1[ll_filadet]  
				dw_spro_cajasprod.Object.emba_codigo[ll_filnew] = ls_embalaje  
				dw_spro_cajasprod.Object.etiq_codigo[ll_filnew] = li_etiqueta  
				dw_spro_cajasprod.Object.capr_fecemb[ll_filnew] = dw_1.Object.pafr_fecemb[ll_filadet]  
				dw_spro_cajasprod.Object.capr_calibr[ll_filnew] = ls_calibre  
				dw_spro_cajasprod.Object.capr_numpal[ll_filnew] = ll_pallet 
				dw_spro_cajasprod.Object.capr_numtra[ll_filnew] = ll_pallet
				dw_spro_cajasprod.Object.capr_estado[ll_filnew] = 1  
				dw_spro_cajasprod.Object.capr_varrot[ll_filnew] = dw_1.Object.pafr_varrot[ll_filadet]  
				dw_spro_cajasprod.Object.cate_codigo[ll_filnew] = dw_1.Object.cate_codigo[ll_filadet]  
				dw_spro_cajasprod.Object.capr_cespak[ll_filnew] = dw_1.Object.pafr_copack[ll_filadet] 
				//dw_spro_cajasprod.Object.capr_docrel[ll_filnew] = dw_1.Object.pafr_docrel[ll_filadet] 
				dw_spro_cajasprod.Object.capr_hordig[ll_filnew] = Now()  
				dw_spro_cajasprod.Object.capr_fecdig[ll_filnew] = Today()  
				//dw_spro_cajasprod.Object.capr_nrlote[ll_filnew] = dw_1.Object.pafr_nrlote[ll_filadet]
				dw_spro_cajasprod.Object.capr_lineas[ll_filnew] = iuo_correl.loco_comlin  
				dw_spro_cajasprod.Object.capr_pcline[ll_filnew] = is_computador
				dw_spro_cajasprod.Object.capr_prdrot[ll_filnew] = ll_prodrot
				
				lb_autocommit = SQLCA.Autocommit
				SQLCA.Autocommit = FALSE
				
				IF dw_spro_cajasprod.Update(True, False) = 1 THEN
					Commit;
						
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, 'Compacto')
						RollBack;
						Return
					ELSE
																
						dw_spro_cajasprod.ResetUpdate()
						
					END IF
				ELSE
					F_ErrorBaseDatos(sqlca, 'Compacto')
					RollBack;
					Return
				END IF
						
				ll_Ncaja ++
									
			NEXT	
		NEXT
		ll_final = ll_ncaja - 1
		
		ll_Fila = 0
					
		ll_Fila1 = dw_2.Retrieve(li_cliente,li_planta,ll_inicial,ll_final,li_mercado)
		IF dw_2.Object.espe_codigo[1] <> 11 THEN
			FOR ll_fill = 1 TO ll_Fila1
				
				ll_nrocaja = dw_2.Object.capr_numero[ll_fill]
				gl_packing = dw_2.Object.plde_codigo[ll_fill]
				
				dw_16.Retrieve(li_cliente,li_planta,ll_nrocaja,ll_nrocaja,li_mercado)
				
				dw_16.Object.Ole_1.Object.BarCode	=	Integer(dw_1.Object.clie_codbar[1])
				IF dw_16.Object.Ole_1.Object.BarCode = 20 THEN
	//				sle_1.Text								=	"Imprimiendo Adhesivos en Formato CodeBar128"
					dw_16.Object.Ole_1.Object.Text 	= 	'00' + &
																	String(dw_1.Object.zona_codigo[1],'00') + &
																	String(dw_1.Object.plde_codigo[1],'0000') + &
																	String(ll_nrocaja,'0000000000')
					
				ELSEIF dw_16.Object.Ole_1.Object.BarCode = 88 THEN
				//	sle_1.Text								=	"Imprimiendo Adhesivos en Formato GS1"
					ls_fecha									=	String(dw_1.Object.pafr_fecemb[1])
					ls_fecha									=	Right(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Left(ls_fecha, 2)
					dw_16.Object.Ole_1.Object.Text 	= 	"01" + dw_1.Object.emba_nrogs1[1] + "10" + ls_fecha + "\F" + &
																	"21" + String(gl_packing, "0000") +  &
																	String(ll_nrocaja, '00000000')
					
				END IF
				dw_16.AcceptText()
				dw_16.Print()	
			NEXT	
		ELSE
				
			FOR ll_fill = 1 TO ll_Fila1 step 2
				
				ll_nrocaja = dw_2.Object.capr_numero[ll_fill]
				gl_packing = dw_2.Object.plde_codigo[ll_fill]
				
				
				ll_inicio	=	Long(dw_2.Object.capr_numero[ll_fill])
			
				IF ll_Fila + 1 <= dw_2.RowCount() THEN
					ll_final		=	Long(dw_2.Object.capr_numero[ll_fill + 1])
				ELSE
					ll_final		=	Long(dw_2.Object.capr_numero[ll_fill])
				END IF
				
				dw_16.Retrieve(li_cliente,li_planta,ll_inicio,ll_final,li_mercado)
				
				dw_16.Object.Ole_1.Object.BarCode	=	Integer(dw_1.Object.clie_codbar[1])
				
				dw_16.Object.Ole_1.Object.BarCode = Integer(dw_1.Object.clie_codbar[1])
				//	sle_1.Text								=	"Imprimiendo Adhesivos en Formato GS1"
				ls_fecha									=	String(dw_1.Object.pafr_fecemb[1])
				ls_fecha									=	Right(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Left(ls_fecha, 2)
				dw_16.Object.Ole_1.Object.Text 	= 	"01" + dw_1.Object.emba_nrogs1[1] + "10" + ls_fecha + "\F" + &
																"21" + String(gl_packing, "0000") +  &
																String(ll_inicio, '00000000')
				
				IF ll_fill + 1 <= dw_2.RowCount() THEN
				
					ll_nrocaja = dw_2.Object.capr_numero[ll_fill + 1]
					gl_packing = dw_2.Object.plde_codigo[ll_fill + 1]
									
					dw_16.Object.Ole_2.Object.Text 	= 	"01" + dw_1.Object.emba_nrogs1[1] + "10" + ls_fecha + "\F" + &
																	"21" + String(gl_packing, "0000") + &
																	String(ll_final, '00000000')
				END IF																		
						
				dw_16.AcceptText()
				dw_16.Print()
			NEXT	
		END IF	
		
		
		
		
		
		
		IF ll_Fila1 = -1 THEN
			MessageBox( "Error en Base de Datos", &
							"Se ha producido un error en Base " + &
							"de Datos : ~n" + sqlca.SQLErrText, StopSign!, OK!)
		ELSEIF ll_Fila1 = 0 THEN
			MessageBox( "No Existe información", &
							"No Existe información para este informe.", &
							StopSign!, OK!)
		ELSE
			
		END IF
	ELSE
		MessageBox( "No Existe información", &
							"No Existe información para este Pallet.", &
							StopSign!, OK!)
		Return 1							
	END IF	
	
	
	
	
	
	
	
	
	
ELSE
	ll_Fila1 = dw_2.Retrieve(li_cliente,li_planta,ll_inicial,ll_final,li_mercado)
	
	IF ll_Fila1 = -1 THEN
		MessageBox( "Error en Base de Datos", &
						"Se ha producido un error en Base " + &
						"de Datos : ~n" + sqlca.SQLErrText, StopSign!, OK!)
	ELSEIF ll_Fila1 = 0 THEN
		MessageBox( "No Existe información", &
						"No Existe información para este informe.", &
						StopSign!, OK!)
	END IF
		
	IF dw_2.Object.espe_codigo[1] <> 11 THEN
		
		FOR ll_fill = 1 TO ll_Fila1
			
			ll_nrocaja = dw_2.Object.capr_numero[ll_fill]
			gl_packing = dw_2.Object.plde_codigo[ll_fill]
			
			dw_16.Retrieve(li_cliente,li_planta,ll_nrocaja,ll_nrocaja,li_mercado)
			
			dw_16.Object.Ole_1.Object.BarCode	=	Integer(dw_1.Object.clie_codbar[1])
			IF dw_16.Object.Ole_1.Object.BarCode = 20 THEN
				dw_16.Object.Ole_1.Object.Text 	= 	'00' + &
																String(dw_1.Object.zona_codigo[1],'00') + &
																String(dw_1.Object.plde_codigo[1],'0000') + &
																String(dw_1.Object.capr_numero[1],'0000000000')
				
			ELSEIF dw_16.Object.Ole_1.Object.BarCode = 88 THEN
				ls_fecha									=	String(dw_1.Object.pafr_fecemb[1])
				ls_fecha									=	Right(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Left(ls_fecha, 2)
				dw_16.Object.Ole_1.Object.Text 	= 	"01" + (dw_1.Object.emba_nrogs1[1]) + "10" + ls_fecha + "\F" + &
																"21" + String(gl_packing, "0000") + '0' + &
																String(ll_nrocaja, '0000000')
				
			END IF
			dw_16.AcceptText()
			dw_16.Print()	
		NEXT	
	ELSE
		
		FOR ll_fill = 1 TO ll_Fila1 STEP 2
			
			ll_nrocaja = dw_2.Object.capr_numero[ll_fill]
			gl_packing = dw_2.Object.plde_codigo[ll_fill]
			
			ll_inicio	=	Long(dw_2.Object.capr_numero[ll_fill])
			
			IF ll_Fila + 1 <= dw_2.RowCount() THEN
				ll_final		=	Long(dw_2.Object.capr_numero[ll_fill + 1])
			ELSE
				ll_final		=	Long(dw_2.Object.capr_numero[ll_fill])
			END IF
			
			dw_16.Retrieve(li_cliente,li_planta,ll_inicio,ll_final,li_mercado)
			
			dw_16.Object.Ole_1.Object.BarCode	=	Integer(dw_1.Object.clie_codbar[1])
			ls_fecha									=	String(dw_1.Object.pafr_fecemb[1])
			ls_fecha									=	Right(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Left(ls_fecha, 2)
			dw_16.Object.Ole_1.Object.Text 	= 	"01" + (dw_1.Object.emba_nrogs1[1]) + "10" + ls_fecha + "\F" + &
																"21" + String(gl_packing, "0000") + '0' + &
																String(ll_inicio, '0000000')
			IF ll_fill + 1 <= dw_2.RowCount() THEN
				
				ll_nrocaja = dw_2.Object.capr_numero[ll_fill + 1]
				gl_packing = dw_2.Object.plde_codigo[ll_fill + 1]
								
				dw_16.Object.Ole_2.Object.Text 	= 	"01" + dw_1.Object.emba_nrogs1[1] + "10" + ls_fecha + "\F" + &
																"21" + String(gl_packing, "0000") + &
																String(ll_final, '00000000')
			END IF																
		
			dw_16.AcceptText()
			dw_16.Print()
			
			
		NEXT	
		
		
	END IF	
END IF	

dw_16.Reset()
dw_2.Reset()
dw_1.reset()
				

end event

type pb_salir from w_para_informes`pb_salir within w_imprecion_de_compactos_pallet_bultos
integer x = 2446
integer y = 708
integer taborder = 60
end type

type st_1 from statictext within w_imprecion_de_compactos_pallet_bultos
integer x = 334
integer y = 528
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_imprecion_de_compactos_pallet_bultos
integer x = 631
integer y = 512
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

	
ELSE
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_imprecion_de_compactos_pallet_bultos
integer x = 334
integer y = 660
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type dw_plantadesp from datawindow within w_imprecion_de_compactos_pallet_bultos
integer x = 631
integer y = 648
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

type em_numero from editmask within w_imprecion_de_compactos_pallet_bultos
integer x = 631
integer y = 908
integer width = 489
integer height = 92
integer taborder = 40
boolean bringtotop = true
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

event modified;istr_mant.argumento[4]	=	This.Text


end event

type st_3 from statictext within w_imprecion_de_compactos_pallet_bultos
integer x = 334
integer y = 916
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Pallet"
boolean focusrectangle = false
end type

type st_5 from statictext within w_imprecion_de_compactos_pallet_bultos
integer x = 251
integer y = 440
integer width = 1989
integer height = 644
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_16 from datawindow within w_imprecion_de_compactos_pallet_bultos
boolean visible = false
integer x = 1152
integer y = 1208
integer width = 581
integer height = 444
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_spro_cajasprod_carozos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_1 from datawindow within w_imprecion_de_compactos_pallet_bultos
boolean visible = false
integer x = 2395
integer y = 1260
integer width = 686
integer height = 400
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_palletfruta_pallet_bultos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_spro_cajasprod from datawindow within w_imprecion_de_compactos_pallet_bultos
boolean visible = false
integer x = 521
integer y = 1676
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

type st_4 from statictext within w_imprecion_de_compactos_pallet_bultos
integer x = 334
integer y = 784
integer width = 283
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Mercado"
boolean focusrectangle = false
end type

type dw_mercado from datawindow within w_imprecion_de_compactos_pallet_bultos
integer x = 626
integer y = 780
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

type dw_2 from datawindow within w_imprecion_de_compactos_pallet_bultos
boolean visible = false
integer x = 2688
integer y = 272
integer width = 686
integer height = 400
boolean bringtotop = true
string title = "none"
string dataobject = "dw_adhesivo"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

