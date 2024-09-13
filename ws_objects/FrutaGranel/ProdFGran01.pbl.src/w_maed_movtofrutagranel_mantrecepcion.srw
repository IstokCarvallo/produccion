$PBExportHeader$w_maed_movtofrutagranel_mantrecepcion.srw
$PBExportComments$Mantención de Recepción de Fruta Granel de Huerto.
forward
global type w_maed_movtofrutagranel_mantrecepcion from w_mant_encab_deta_csd
end type
type dw_4 from datawindow within w_maed_movtofrutagranel_mantrecepcion
end type
type cb_guia from commandbutton within w_maed_movtofrutagranel_mantrecepcion
end type
type dw_9 from datawindow within w_maed_movtofrutagranel_mantrecepcion
end type
type dw_exidetaborra from datawindow within w_maed_movtofrutagranel_mantrecepcion
end type
type dw_exismovtodetanulos from datawindow within w_maed_movtofrutagranel_mantrecepcion
end type
type dw_exideta from datawindow within w_maed_movtofrutagranel_mantrecepcion
end type
type dw_exiencab from datawindow within w_maed_movtofrutagranel_mantrecepcion
end type
type dw_envases_comer from datawindow within w_maed_movtofrutagranel_mantrecepcion
end type
type dw_lotescategoria from datawindow within w_maed_movtofrutagranel_mantrecepcion
end type
type dw_pdf from uo_dw within w_maed_movtofrutagranel_mantrecepcion
end type
type dw_docto from uo_dw within w_maed_movtofrutagranel_mantrecepcion
end type
type dw_desverd from datawindow within w_maed_movtofrutagranel_mantrecepcion
end type
type dw_spro_bins from datawindow within w_maed_movtofrutagranel_mantrecepcion
end type
type ole_puerta from olecustomcontrol within w_maed_movtofrutagranel_mantrecepcion
end type
type tab_1 from tab within w_maed_movtofrutagranel_mantrecepcion
end type
type tp_1 from userobject within tab_1
end type
type dw_lotes from uo_dw within tp_1
end type
type tp_1 from userobject within tab_1
dw_lotes dw_lotes
end type
type tp_2 from userobject within tab_1
end type
type dw_envrec from uo_dw within tp_2
end type
type tp_2 from userobject within tab_1
dw_envrec dw_envrec
end type
type tp_3 from userobject within tab_1
end type
type dw_envdes from uo_dw within tp_3
end type
type tp_3 from userobject within tab_1
dw_envdes dw_envdes
end type
type tab_1 from tab within w_maed_movtofrutagranel_mantrecepcion
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
end type
type dw_6 from datawindow within w_maed_movtofrutagranel_mantrecepcion
end type
type str_productores_envases from structure within w_maed_movtofrutagranel_mantrecepcion
end type
type str_pesaje from structure within w_maed_movtofrutagranel_mantrecepcion
end type
type str_envase from structure within w_maed_movtofrutagranel_mantrecepcion
end type
end forward

type str_productores_envases from structure
	long		productor[]
	long		guiasii[]
	integer		tipomovto[]
	long		numero[]
end type

type str_pesaje from structure
	datetime		fechahora[]
	decimal { 4 }		pesaje[]
	decimal { 4 }		total
	boolean		agrega
	boolean		modifica
	str_puertacomm		puerta
	datawindow		dw
	string		argum[]
end type

type str_envase from structure
	integer		tipenv
	integer		codenv
	string		calcos
	integer		cantid
end type

global type w_maed_movtofrutagranel_mantrecepcion from w_mant_encab_deta_csd
integer width = 4951
integer height = 2208
string title = "MANTENCION RECEPCION DE HUERTO"
string menuname = ""
boolean minbox = false
boolean clientedge = true
event ue_validapassword ( )
event type long ue_despuesguardar ( )
event ue_despuesborrar ( )
event ue_despuesborrar2 ( )
event type long ue_despuesguardar2 ( )
event anulamovto ( )
event anulamovto2 ( )
dw_4 dw_4
cb_guia cb_guia
dw_9 dw_9
dw_exidetaborra dw_exidetaborra
dw_exismovtodetanulos dw_exismovtodetanulos
dw_exideta dw_exideta
dw_exiencab dw_exiencab
dw_envases_comer dw_envases_comer
dw_lotescategoria dw_lotescategoria
dw_pdf dw_pdf
dw_docto dw_docto
dw_desverd dw_desverd
dw_spro_bins dw_spro_bins
ole_puerta ole_puerta
tab_1 tab_1
dw_6 dw_6
end type
global w_maed_movtofrutagranel_mantrecepcion w_maed_movtofrutagranel_mantrecepcion

type variables
w_mant_deta_lotesfrutagranel_recepcion	iw_mantencion_1
w_mant_deta_movtoenvadeta_recepfruta	iw_mantencion_2

DataWindowChild								idwc_Transp,idwc_Camion,idwc_Variedad,idwc_Predio,&
													idwc_Camara,idwc_especie,idwc_especiedet,idwc_especiedet1,&
													idwc_especiedet2,idwc_planta,idwc_plantadw4,idwc_plancodw4,&
													idwc_plantadw1,idwc_Cliente
DataWindow									dw_3,dw_5,dw_7

Str_mant											istr_mant3, istr_mant4
uo_transportista								iuo_Transport
uo_camiones									iuo_Camion
uo_especie										iuo_Especie
uo_Productores									iuo_Productor
uo_LotesCorrel									iuo_Correlativo
uo_pesoestanespe								iuo_PesoEstanEspe
uo_fechaMovto									iuo_FechaMovto
uo_tipomovtofruta								iuo_TipoMovtoFruta
uo_tipomovtofruta								iuo_TipoMovtoEnva
uo_calicosechero 								iuo_calicosechero
uo_bins											iuo_bins
uo_RecepcionhuertoPDF						iuo_HuertoPDF
uo_grabatablabitacora						iuo_grabatablabitacora

Long     											il_NumFruta=0, il_NumEnva=0, il_lotes[], il_numeroenva, li_retorno, il_coderror

Boolean											ib_AutoCommit, ib_Salida, ib_Destare, ib_ocx,&
													ib_graba_destare, ib_ConectadoExistencia
													
String												is_rut, is_rutprod, is_RutProductor, is_NombreProductor, &
													is_chofer, is_correo, is_error
													
Integer											ii_Cantidad, ii_Productor, ii_kildec=0, ii_especie, &
													ii_TipMov, il_conexiste, il_coneccion, il_packing
													
DateTime											idt_FechaSistema
Transaction										sqlexi

String												is_enva_tipoen[], is_enva_codigo[], is_cale_calida[], &
													is_cantidad[], 	is_pesone[], 		is_cale_nombre[]
						
Private:
str_Productores_Envases						wstr_Prod_Enva
str_pesaje              							wstr_pesaje
str_pesaje              							wstr_pesajeCarro
str_puertacomm		      					istr_puertacomm
end variables

forward prototypes
public subroutine determina_productoresenvase (integer ai_tipomovto)
public subroutine productoreslotes (ref string productores[])
protected function integer wf_modifica ()
public function boolean chequea_envasesproductor ()
public function boolean otrosmovtosdetalle ()
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine captura_totales ()
public function boolean actual_ultimo_lote ()
public function boolean verificalote (integer ai_planta, integer ai_especie, long al_lote)
public function boolean verificabultos (integer ai_planta, integer ai_loteplanta, integer ai_especie, integer ai_lotecod, integer ai_bultos)
public subroutine destare (boolean ab_actualiza)
public subroutine habilitasalida ()
public subroutine habilitaencab (boolean habilita)
public function boolean existerecepcion (integer ai_planta, integer ai_tipomovto, integer al_numero, integer ai_cliente)
public function boolean noexistecliente (integer cliente)
public subroutine actualizakiloslote ()
public subroutine binsapalletfruta ()
public subroutine asignapeso ()
public function boolean procesopacking ()
public function boolean conexionexistencia ()
public function boolean chequea_binscontraenvases ()
public subroutine codigoconexion ()
public subroutine updatemovtogranpesa ()
public function boolean packingproductor (long al_productor)
public subroutine wf_productoreslotes (ref string productores[])
public function boolean wf_grabaregistro ()
public function boolean wf_creaarchivo (string as_archivo)
public function boolean wf_generadoctopdf ()
public function boolean wf_validalotectlcal ()
public subroutine cargamovenv ()
public subroutine cargaenvases ()
public function boolean eliminacion_packing ()
public function boolean datos_correo ()
public function any tipo_update_lote (long al_planta, integer ai_especie, long al_lote, integer ai_tipenv, integer ai_codenv)
end prototypes

event ue_validapassword();str_mant	lstr_mant

lstr_mant.Argumento[1]	=	"Granel"
lstr_mant.Argumento[2]	=	gstr_paramplanta.Password

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

event type long ue_despuesguardar();integer	li_bodega, li_fila, li_cliente, li_enva_codigo, li_enva_tipoen, &
			li_bodevirtual, li_bodzonal, li_planta, li_tipomov, li_lote, li_categoria, li_envase,li_tipoenva,li_bodecomercial, li_devcorreo
Long 		ll_fila, ll_numero, li_secuencia = 1, ll_numnuevoini, ll_numnuevofin, ll_count, ll_docrel, &
			ll_recepcion, ll_filote, ll_filas, ll_Filexiste, ll_filaenva, ll_cantidad, ll_secuencia2
String	ls_item_codigo, ls_productor, ls_prod, ls_calidad, ls_calilote, ls_correozonal, ls_correo, ls_texto, ls_asunto, ls_error, serrormsg
Boolean	lb_AutoCommit, lb_Retorno
Date		ld_fecha

IF ib_Conectadoexistencia = TRUE THEN
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	dw_envases_comer.SetTransObject(sqlca)
	dw_lotescategoria.SetTransObject(sqlca)
	
	li_cliente 	= dw_2.Object.clie_codigo[1]
	li_planta 	= dw_2.Object.plde_codigo[1] 
	li_tipomov	= dw_2.Object.tpmv_codigo[1]
	ll_recepcion = dw_2.Object.mfge_numero[1]
	
	dw_lotescategoria.GetChild("vari_codigo",idwc_Variedad)
	idwc_Variedad.SetTransObject(SQLCA)
	idwc_Variedad.Retrieve(dw_2.Object.espe_codigo[1])

	dw_lotescategoria.Retrieve(li_planta,li_tipomov,ll_recepcion,li_cliente)
		
	FOR ll_filote = 1 TO dw_lotescategoria.RowCount()
		
		li_lote			= dw_lotescategoria.Object.lote_codigo[ll_filote]
		li_categoria 	= dw_lotescategoria.Object.cate_codigo[ll_filote]
		
		dw_envases_comer.Retrieve(li_planta,li_lote,ll_recepcion,li_tipomov,li_cliente)
		
		FOR ll_filaenva = 1 TO dw_envases_comer.RowCount()
//			IF li_categoria = 101 THEN
//				//ll_secuencia2 = li_secuencia + 1
//				li_secuencia = 1 
//			END IF
			
			IF li_secuencia = 1 THEN
								
				IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
					Message.DoubleParm = -1
					li_retorno = 2
					is_error = 'Error en Parámetros de Existencia'
					RETURN 2
				ELSE
					li_bodevirtual 	= luo_existencia.bodvirtual
					li_bodzonal			= luo_existencia.bodzonal
					li_bodecomercial 	= luo_existencia.bodcomercial
				END IF	
							
				ll_docrel = dw_2.Object.mfge_numero[1]
				
//				IF li_categoria = 101 THEN
//					li_bodevirtual 	= luo_existencia.bodcomercial				
//				END IF	
				
				IF luo_existencia.Mesproceso > dw_2.Object.mfge_fecmov[1] THEN
					Message.DoubleParm = -1
					
					ll_docrel = dw_2.Object.mfge_numero[1]
					
					ld_fecha			= dw_2.Object.mfge_fecmov[1]
			
					IF luo_existencia.maximo_porfecha(1,li_bodzonal,ll_docrel,1,ld_fecha,True,sqlexi) THEN
						ll_numero = luo_existencia.numero
					END IF
					
					IF isnull(ll_numero) THEN
						ll_numero = 0
					END IF	
							
					IF luo_existencia.bodega_zonal(li_bodzonal,sqlexi,True) THEN
						ls_correozonal = luo_existencia.correozonal
					END IF	
					
					ls_correo = luo_existencia.correo
					
					IF ll_numero = 0 THEN
						ls_texto	 = "No es posible actualizar movto. en Existencia, por modificación anterior al mes de proceso(No presenta movimiento en bodegas)"
					ELSE	
						ls_texto	 = "No es posible actualizar movto. en Existencia Nº "+String(ll_numero)+"; por modificación anterior al mes de proceso"
					END IF
					
					ls_asunto = "Modifica Recepción fruta granel, Movto. Nº "+String(ll_docrel)
					//li_devcorreo =  Send_mail("smtp.rioblanco.cl","<frutaGranel@rioblanco.cl>","<"+ls_correo+">","","",ls_asunto,ls_texto,"",ls_error)
//					
//					IF (li_devcorreo<0) THEN
//						messagebox("Error No" + string(li_devcorreo),sErrorMsg)
//					END IF
					li_retorno = 2
					is_error = ls_texto
					RETURN 1
				END IF	
				
				luo_existencia.existeregistro(ll_docrel,li_bodzonal,1,1,True,sqlexi,dw_2.Object.mfge_fecmov[1])
				
				IF luo_existencia.count = 0 THEN
					IF Not luo_existencia.correlativobode(1,li_bodzonal,li_bodzonal,True,sqlexi) THEN
						Message.DoubleParm = -1
						li_retorno = 2
						is_error = 'Problema con Correlativos de bodega'
						RETURN 2
					ELSE
						ll_numero = luo_existencia.numero
					END IF	
							
					IF isnull(ll_numero) THEN
						ll_numnuevoini = Long(String(li_bodzonal)+''+'0001')
						ll_numnuevofin = Long(String(li_bodzonal)+''+'9999')
						
						INSERT INTO dba.correlbode(bode_codigo,mden_tipdoc,cobo_bodadm,cobo_inicia,cobo_final,cobo_actual)
						VALUES(:li_bodzonal,1,:li_bodzonal,:ll_numnuevoini,:ll_numnuevofin,:ll_numnuevoini)
						USING sqlexi;
						
						IF sqlexi.SQLCode = -1 THEN
							F_ErrorBaseDatos(sqlexi,"Correlbode")
							Message.DoubleParm = -1
							sqlexi.AutoCommit	=	False
							RETURN 2
						END IF
						ll_numero = ll_numnuevoini - 1
					END IF	
					ll_numero = ll_numero + 1
					
					ls_productor = String(dw_4.Object.prod_codigo[1],'000000')
							
					IF Not luo_existencia.Existecliente(ls_productor,True,sqlexi) THEN
						Message.DoubleParm = -1
						li_retorno = 2
						is_error = 'Error en Tabla Cliente en existencia, es Posible que Cliente no Exista '+ls_productor
						RETURN 2
						ls_productor = luo_existencia.prod
					END IF
					
					IF luo_existencia.prod = '' OR isnull(luo_existencia.prod) THEN
						ls_productor = luo_existencia.prdgen
					END IF	
					
					IF isnull(dw_3.Object.lote_prdpak[1]) THEN
						IF NOT packingproductor(long(ls_productor)) THEN
							il_packing = dw_3.Object.lote_prdpak[1]
							//correo tabla bodegas
							IF luo_existencia.bodega_zonal(li_bodzonal,sqlexi,True) THEN
								ls_correo = luo_existencia.correozonal
							END IF	
							
							IF isnull(il_packing) OR il_packing = 0 THEN
								ls_texto	 = "Movto. Recepción Nº "+ string(ll_numero) +", NO Genera Movto. en Existencia. Debe Realizar Mantención Productor-Packing."
								ls_asunto = "Falta Packing en Recepción"	
							ELSE	
								ls_texto	 = "Falta Código de Packing Nº " + String(il_packing) +", Debe Realizar Mantención Productor-Packing"
								ls_asunto = "Falta Packing en Tabla"
							END IF							
													
//							li_devcorreo =  Send_mail("smtp.rioblanco.cl","<frutaGranel@rioblanco.cl>","<"+ls_correo+">","","",ls_asunto,ls_texto,"",ls_error)
							
							IF isnull(il_packing) OR il_packing = 0 THEN
								Return 2
							END IF	
						END IF	
					ELSE
						il_packing = dw_3.Object.lote_prdpak[1]	
					END IF
					
					luo_existencia.bodega_administradora(il_packing,True, sqlexi)
					
					IF luo_existencia.bodeAdmi <> luo_existencia.bodzonal THEN
						//Pato pide que se haga asi 2014/12/15
						il_packing = luo_existencia.bodeAdmi
						
						SELECT isnull(plde_conpro,0)
						INTO :il_coneccion 
						FROM dbo.plantadesp 
						WHERE plde_codigo = :il_packing;
						
						IF il_coneccion = 0 THEN
							il_coneccion = 95
						END IF	
					ELSE
						SELECT 	cone_codigo
						INTO   	:il_coneccion
						FROM dbo.clientesprod
						WHERE clie_codigo = :li_Cliente;
					END IF
					
					dw_exiencab.Object.mden_tipdoc[1] = 1
					dw_exiencab.Object.mden_numero[1] = ll_numero 
					dw_exiencab.Object.tpdo_codigo[1] = 3
					dw_exiencab.Object.mden_fecmov[1] = dw_2.Object.mfge_fecmov[1]
					dw_exiencab.Object.tpmv_tipomv[1] = 1
					dw_exiencab.Object.tpmv_codigo[1] = 16
					dw_exiencab.Object.mden_tipana[1] = 3
					dw_exiencab.Object.mden_estaci[1] = gstr_us.computador
					dw_exiencab.Object.mden_fecdig[1] = Date(Today())
					dw_exiencab.Object.mden_hordig[1] = Time(Now())
					
					dw_exiencab.Object.mden_gdprod[1] = dw_4.Object.meen_guisii[1]
					
					//IF li_categoria <> 101 THEN
						dw_exiencab.Object.bode_codigo[1] = li_bodzonal
					//ELSE
					//	dw_exiencab.Object.bode_codigo[1] = luo_existencia.bodcomercial
					//END IF 
					
					dw_exiencab.Object.mden_bodest[1] = il_packing
					dw_exiencab.Object.clpr_rut[1]	 = ls_productor
					dw_exiencab.Object.mden_docrel[1] = dw_2.Object.mfge_numero[1]
					dw_exiencab.Object.mden_fecdre[1] = dw_2.Object.mfge_fecmov[1]
					dw_exiencab.Object.mden_observ[1] = 'Recepción de Envases Fruta Granel'
					dw_exiencab.Object.mden_estado[1] = 1
					dw_exiencab.Object.mden_pcopda[1] = 1
				ELSE
					luo_existencia.numero_actual(ll_docrel,li_bodzonal,1,1,True,sqlexi,dw_2.Object.mfge_fecmov[1])
					ll_numero = luo_existencia.numeroactual
				END IF									
			END IF							
									
			li_enva_codigo = dw_envases_comer.Object.enva_codigo[ll_filaenva]
			li_enva_tipoen = dw_envases_comer.Object.enva_tipoen[ll_filaenva]
			ls_calidad		= dw_envases_comer.Object.cale_calida[ll_filaenva]
			ll_cantidad		= dw_envases_comer.Object.cantidad[ll_filaenva]
													
			ll_fila	=	dw_exideta.InsertRow(0)
									
			IF Not iuo_calicosechero.Existe(li_enva_tipoen,li_enva_codigo,ls_calidad,True,sqlca) THEN
				Message.DoubleParm = -1
				li_retorno = 2
				is_error = 'Problema con envases'
				RETURN 2
			ELSE
				ls_item_codigo = iuo_calicosechero.item 
			END IF	
			
			IF ls_item_codigo = '' OR IsNull(ls_item_codigo) THEN
				ls_item_codigo = luo_existencia.itgene
			END IF
							
			dw_exideta.Object.mden_tipdoc[ll_fila] = 1
			dw_exideta.Object.mden_numero[ll_fila] = ll_numero
			ll_secuencia2 ++
//			IF li_categoria = 101 THEN
//				dw_exideta.Object.mdde_secuen[ll_fila] = ll_secuencia2 
//			ELSE
				dw_exideta.Object.mdde_secuen[ll_fila] = li_secuencia 
//			END IF
			dw_exideta.Object.tpmv_tipomv[ll_fila] = 1 
			dw_exideta.Object.tpmv_codigo[ll_fila] = 16
			dw_exideta.Object.item_codigo[ll_fila] = ls_item_codigo
			dw_exideta.Object.mdde_identi[ll_fila] = ''
			dw_exideta.Object.mdde_fecmov[ll_fila] = dw_2.Object.mfge_fecmov[1]
			dw_exideta.Object.bode_codigo[ll_fila] = li_bodzonal
			dw_exideta.Object.mdde_cantid[ll_fila] = ll_cantidad
						
			li_secuencia = li_secuencia + 1
				
		NEXT		
		IF dw_exiencab.Rowcount() > 0 OR li_secuencia > 1 THEN
			lb_AutoCommit		=	sqlexi.AutoCommit
			sqlexi.AutoCommit	=	False
		
			IF dw_exiencab.Update(True, False) = 1 THEN
				IF dw_exideta.Update(True, False) = 1 THEN
					Commit;
					
					IF sqlexi.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlexi, This.Title)
						
						RollBack;
						li_retorno = 2	
						//Messagebox("Existencia","Grabación de Datos NO se realizó")
					ELSE
						li_retorno = 1						
						dw_exiencab.ResetUpdate()
						dw_exideta.ResetUpdate()
						//Messagebox("Existencia","Grabación de Datos realizada Satisfactoriamente")
						
					END IF
				ELSE
									
					RollBack;
					li_retorno = 2
					//Messagebox("Existencia","Grabación de Datos NO se realizó")
				END IF
			ELSE
				F_ErrorBaseDatos(sqlexi, This.Title)
			
				RollBack;
				li_retorno = 2	
				//Messagebox("Existencia","Grabación de Datos NO se realizó")
			END IF
				
			dw_exiencab.Reset()
			dw_exideta.Reset()
		END IF
	NEXT	
	
	pb_grabar.Enabled = False
	dw_exiencab.Reset()
	dw_exideta.Reset()
	sqlexi.AutoCommit	=	True
   Return li_retorno
END IF	
end event

event ue_despuesborrar();Long 		ll_numero, respuesta, ll_nueva, ll_fila, ll_filote, ll_filaenva
Boolean lb_AutoCommit
Integer	li_bodevirtual, li_bodzonal, li_bodecomercial, li_cliente, li_planta, li_tipomov, &
		ll_recepcion, li_lote, li_categoria 	
Date	ld_fecha
String	ls_productor

IF ib_Conectadoexistencia = TRUE THEN
	
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
	//	sqlexi.AutoCommit	=	ib_AutoCommit
		RETURN 
	ELSE
		li_bodevirtual 	= luo_existencia.bodvirtual
		li_bodzonal			= luo_existencia.bodzonal
		li_bodecomercial 	= luo_existencia.bodcomercial
	END IF	
		
	IF luo_existencia.Mesproceso > dw_2.Object.mfge_fecmov[1] THEN
		Message.DoubleParm = -1
		RETURN 
	END IF	
	
	dw_envases_comer.SetTransObject(sqlca)
	dw_lotescategoria.SetTransObject(sqlca)
	
	dw_lotescategoria.GetChild("vari_codigo",idwc_Variedad)
	idwc_Variedad.SetTransObject(SQLCA)
	idwc_Variedad.Retrieve(dw_2.Object.espe_codigo[1])
	
	li_cliente 		= dw_2.Object.clie_codigo[1]
	li_planta 		= dw_2.Object.plde_codigo[1] 
	li_tipomov		= dw_2.Object.tpmv_codigo[1]
	ll_recepcion 	= dw_2.Object.mfge_numero[1]

	dw_lotescategoria.Retrieve(li_planta,li_tipomov,ll_recepcion,li_cliente)
	
	ls_productor = String(dw_4.Object.prod_codigo[1],'000000')
							
	IF Not luo_existencia.Existecliente(ls_productor,True,sqlexi) THEN
		Message.DoubleParm = -1
		RETURN 
		ls_productor = luo_existencia.prod
	END IF
	
	IF luo_existencia.prod = '' OR isnull(luo_existencia.prod) THEN
		ls_productor = luo_existencia.prdgen
	END IF	
	
	IF NOT packingproductor(long(ls_productor)) THEN
		il_packing = dw_3.Object.lote_prdpak[1]
	END IF	
	
	FOR ll_filote = 1 TO dw_3.RowCount()
		
		li_lote			= dw_3.Object.lote_codigo[ll_filote]
		li_categoria 	= dw_3.Object.cate_codigo[ll_filote]
		
		dw_envases_comer.Retrieve(li_planta,li_lote,ll_recepcion,li_tipomov,li_cliente)
		
		FOR ll_filaenva = 1 TO dw_envases_comer.RowCount()
		
//			IF li_categoria = 101 THEN
//				li_bodevirtual 	= luo_existencia.bodcomercial				
//			END IF
				
			il_numeroenva = dw_2.Object.mfge_numero[1]
			ld_fecha			= dw_2.Object.mfge_fecmov[1]
			
			IF NOT luo_existencia.maximo_porfecha(1,li_bodzonal,il_numeroenva,1,ld_fecha,True,sqlexi) THEN
				Message.DoubleParm = -1
				Return
			ELSE
				ll_numero = luo_existencia.numero
			END IF
					
			IF NOT isnull(ll_numero) THEN
					//Return
				
				IF NOT luo_existencia.actualizaexistencia(2,1,li_bodzonal,ll_numero,il_numeroenva,True,sqlexi) THEN
					Message.DoubleParm = -1
					Return
				END IF	
				
				dw_exidetaborra.Retrieve(1,ll_numero)
				
				FOR ll_fila = 1 TO dw_exidetaborra.RowCount()
					ll_nueva = dw_exismovtodetanulos.InsertRow(0)
					
					dw_exismovtodetanulos.Object.mden_tipdoc[ll_nueva] = dw_exidetaborra.Object.mden_tipdoc[ll_fila]   
					dw_exismovtodetanulos.Object.mden_numero[ll_nueva] = dw_exidetaborra.Object.mden_numero[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_secuen[ll_nueva] = dw_exidetaborra.Object.mdde_secuen[ll_fila]  
					dw_exismovtodetanulos.Object.tpmv_tipomv[ll_nueva] = dw_exidetaborra.Object.tpmv_tipomv[ll_fila]  
					dw_exismovtodetanulos.Object.tpmv_codigo[ll_nueva] = dw_exidetaborra.Object.tpmv_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.item_codigo[ll_nueva] = dw_exidetaborra.Object.item_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.item_armado[ll_nueva] = dw_exidetaborra.Object.item_armado[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_identi[ll_nueva] = dw_exidetaborra.Object.mdde_identi[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_fecmov[ll_nueva] = dw_exidetaborra.Object.mdde_fecmov[ll_fila]  
					dw_exismovtodetanulos.Object.bode_codigo[ll_nueva] = dw_exidetaborra.Object.bode_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.ocen_numero[ll_nueva] = dw_exidetaborra.Object.mdde_ocompr[ll_fila]  
					dw_exismovtodetanulos.Object.ccon_codigo[ll_nueva] = dw_exidetaborra.Object.ccon_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.exic_codigo[ll_nueva] = dw_exidetaborra.Object.exic_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_cantid[ll_nueva] = dw_exidetaborra.Object.mdde_cantid[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_canarm[ll_nueva] = dw_exidetaborra.Object.mdde_canarm[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_prunmo[ll_nueva] = dw_exidetaborra.Object.mdde_prunmo[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_totmor[ll_nueva] = dw_exidetaborra.Object.mdde_totmor[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_totpes[ll_nueva] = dw_exidetaborra.Object.mdde_totpes[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_facpes[ll_nueva] = dw_exidetaborra.Object.mdde_facpes[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_facmor[ll_nueva] = dw_exidetaborra.Object.mdde_facmor[ll_fila]  
			
				NEXT
				
				DELETE FROM dba.exismovtodeta 
					WHERE	mden_numero = :ll_numero
					AND	mden_tipdoc = 1
					AND	bode_codigo = :li_bodzonal
					USING sqlexi;
				
				IF sqlexi.SQLCode = -1 THEN
					F_ErrorBaseDatos(sqlexi,"exismovtodeta")
					Message.DoubleParm = -1
					sqlexi.AutoCommit	=	ib_AutoCommit
					RETURN 
				END IF
				
				IF dw_exismovtodetanulos.Rowcount() > 0 THEN
					lb_AutoCommit		=	sqlexi.AutoCommit
					sqlexi.AutoCommit	=	False
					
					IF dw_exismovtodetanulos.Update(True, False) = 1 THEN
						
						dw_exismovtodetanulos.ResetUpdate()
			
					ELSE
						F_ErrorBaseDatos(sqlexi, This.Title)
							
						RollBack;
						Return
					END IF
					
					sqlexi.AutoCommit	=	lb_AutoCommit
				
				END IF
			
	
			END IF
		NEXT	
	NEXT
	//DISCONNECT USING sqlexi;
	//ib_Conectadoexistencia = False
END IF

end event

event ue_despuesborrar2();Long 		ll_numero, respuesta, ll_nueva, ll_fila, ll_filote, ll_filaenva
Boolean lb_AutoCommit
Integer	li_bodevirtual, li_bodzonal, li_bodecomercial, li_cliente, li_planta, li_tipomov, &
		ll_recepcion, li_lote, li_categoria 	
Date	ld_fecha

IF ib_Conectadoexistencia = TRUE THEN
	
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
	//	sqlexi.AutoCommit	=	ib_AutoCommit
		RETURN 
	ELSE
		li_bodevirtual 	= luo_existencia.bodvirtual
		li_bodzonal			= luo_existencia.bodzonal
		li_bodecomercial 	= luo_existencia.bodcomercial
	END IF	
		
	IF luo_existencia.Mesproceso > dw_2.Object.mfge_fecmov[1] THEN
		Message.DoubleParm = -1
		RETURN 
	END IF	
	
	dw_envases_comer.SetTransObject(sqlca)
	dw_lotescategoria.SetTransObject(sqlca)
	
	dw_lotescategoria.GetChild("vari_codigo",idwc_Variedad)
	idwc_Variedad.SetTransObject(SQLCA)
	idwc_Variedad.Retrieve(dw_2.Object.espe_codigo[1])
	
	li_cliente 		= dw_2.Object.clie_codigo[1]
	li_planta 		= dw_2.Object.plde_codigo[1] 
	li_tipomov		= dw_2.Object.tpmv_codigo[1]
	ll_recepcion 	= dw_2.Object.mfge_numero[1]

	dw_lotescategoria.Retrieve(li_planta,li_tipomov,ll_recepcion,li_cliente)
	
	FOR ll_filote = 1 TO dw_3.RowCount()
		
		li_lote			= dw_3.Object.lote_codigo[ll_filote]
		li_categoria 	= dw_3.Object.cate_codigo[ll_filote]
		
		dw_envases_comer.Retrieve(li_planta,li_lote,ll_recepcion,li_tipomov,li_cliente)
		
		FOR ll_filaenva = 1 TO dw_envases_comer.RowCount()
		
//			IF li_categoria = 101 THEN
//				li_bodevirtual 	= luo_existencia.bodcomercial				
//			END IF
				
			il_numeroenva = dw_2.Object.mfge_numero[1]
			ld_fecha			= dw_2.Object.mfge_fecmov[1]
			
			IF NOT luo_existencia.maximo_porfecha(3,il_packing,il_numeroenva,1,ld_fecha,True,sqlexi) THEN
				Message.DoubleParm = -1
				Return
			ELSE
				ll_numero = luo_existencia.numero
			END IF
					
			IF NOT isnull(ll_numero) THEN
					//Return
				
				IF NOT luo_existencia.actualizaexistencia(2,3,il_packing,ll_numero,il_numeroenva,True,sqlexi) THEN
					Message.DoubleParm = -1
					Return
				END IF	
				
				dw_exidetaborra.Retrieve(1,ll_numero)
				
				FOR ll_fila = 1 TO dw_exidetaborra.RowCount()
					ll_nueva = dw_exismovtodetanulos.InsertRow(0)
					
					dw_exismovtodetanulos.Object.mden_tipdoc[ll_nueva] = dw_exidetaborra.Object.mden_tipdoc[ll_fila]   
					dw_exismovtodetanulos.Object.mden_numero[ll_nueva] = dw_exidetaborra.Object.mden_numero[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_secuen[ll_nueva] = dw_exidetaborra.Object.mdde_secuen[ll_fila]  
					dw_exismovtodetanulos.Object.tpmv_tipomv[ll_nueva] = dw_exidetaborra.Object.tpmv_tipomv[ll_fila]  
					dw_exismovtodetanulos.Object.tpmv_codigo[ll_nueva] = dw_exidetaborra.Object.tpmv_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.item_codigo[ll_nueva] = dw_exidetaborra.Object.item_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.item_armado[ll_nueva] = dw_exidetaborra.Object.item_armado[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_identi[ll_nueva] = dw_exidetaborra.Object.mdde_identi[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_fecmov[ll_nueva] = dw_exidetaborra.Object.mdde_fecmov[ll_fila]  
					dw_exismovtodetanulos.Object.bode_codigo[ll_nueva] = dw_exidetaborra.Object.bode_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.ocen_numero[ll_nueva] = dw_exidetaborra.Object.mdde_ocompr[ll_fila]  
					dw_exismovtodetanulos.Object.ccon_codigo[ll_nueva] = dw_exidetaborra.Object.ccon_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.exic_codigo[ll_nueva] = dw_exidetaborra.Object.exic_codigo[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_cantid[ll_nueva] = dw_exidetaborra.Object.mdde_cantid[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_canarm[ll_nueva] = dw_exidetaborra.Object.mdde_canarm[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_prunmo[ll_nueva] = dw_exidetaborra.Object.mdde_prunmo[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_totmor[ll_nueva] = dw_exidetaborra.Object.mdde_totmor[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_totpes[ll_nueva] = dw_exidetaborra.Object.mdde_totpes[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_facpes[ll_nueva] = dw_exidetaborra.Object.mdde_facpes[ll_fila]  
					dw_exismovtodetanulos.Object.mdde_facmor[ll_nueva] = dw_exidetaborra.Object.mdde_facmor[ll_fila]  
			
				NEXT
				
				DELETE FROM dba.exismovtodeta 
					WHERE	mden_numero = :ll_numero
					AND	mden_tipdoc = 3
					AND	bode_codigo = :il_packing
					USING sqlexi;
				
				IF sqlexi.SQLCode = -1 THEN
					F_ErrorBaseDatos(sqlexi,"exismovtodeta")
					Message.DoubleParm = -1
					sqlexi.AutoCommit	=	ib_AutoCommit
					RETURN 
				END IF
				
				IF dw_exismovtodetanulos.Rowcount() > 0 THEN
					lb_AutoCommit		=	sqlexi.AutoCommit
					sqlexi.AutoCommit	=	False
					
					IF dw_exismovtodetanulos.Update(True, False) = 1 THEN
						
						dw_exismovtodetanulos.ResetUpdate()
			
					ELSE
						F_ErrorBaseDatos(sqlexi, This.Title)
							
						RollBack;
						Return
					END IF
					
					sqlexi.AutoCommit	=	lb_AutoCommit
				
				END IF
			
	
			END IF
		NEXT	
	NEXT
	//DISCONNECT USING sqlexi;
	//ib_Conectadoexistencia = False
END IF

end event

event type long ue_despuesguardar2();integer	li_bodega, li_fila, li_cliente, li_enva_codigo, li_enva_tipoen, &
			li_bodevirtual, li_bodzonal, li_planta, li_tipomov, li_lote, li_categoria, li_envase,li_tipoenva,li_bodecomercial, li_devcorreo
Long 		ll_fila, ll_numero, li_secuencia = 1, ll_numnuevoini, ll_numnuevofin, ll_count, ll_docrel, &
			ll_recepcion, ll_filote, ll_filas, ll_Filexiste, ll_filaenva, ll_cantidad, ll_secuencia2
String	ls_item_codigo, ls_productor, ls_prod, ls_calidad, ls_calilote, ls_correozonal, ls_correo, ls_texto, ls_asunto, ls_error, serrormsg
Boolean	lb_AutoCommit, lb_Retorno
Date		ld_fecha

IF ib_Conectadoexistencia = TRUE THEN
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	dw_envases_comer.SetTransObject(sqlca)
	dw_lotescategoria.SetTransObject(sqlca)
	
	li_cliente 	= dw_2.Object.clie_codigo[1]
	li_planta 	= dw_2.Object.plde_codigo[1] 
	li_tipomov	= dw_2.Object.tpmv_codigo[1]
	ll_recepcion = dw_2.Object.mfge_numero[1]
	
	dw_lotescategoria.GetChild("vari_codigo",idwc_Variedad)
	idwc_Variedad.SetTransObject(SQLCA)
	idwc_Variedad.Retrieve(dw_2.Object.espe_codigo[1])

	dw_lotescategoria.Retrieve(li_planta,li_tipomov,ll_recepcion,li_cliente)
		
	FOR ll_filote = 1 TO dw_lotescategoria.RowCount()
		
		li_lote			= dw_lotescategoria.Object.lote_codigo[ll_filote]
		li_categoria 	= dw_lotescategoria.Object.cate_codigo[ll_filote]
		
		dw_envases_comer.Retrieve(li_planta,li_lote,ll_recepcion,li_tipomov,li_cliente)
		
		FOR ll_filaenva = 1 TO dw_envases_comer.RowCount()
//			IF li_categoria = 101 THEN
//				//ll_secuencia2 = li_secuencia + 1
//				li_secuencia = 1 
//			END IF
			
			IF li_secuencia = 1 THEN
								
				IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
					Message.DoubleParm = -1
					li_retorno = 2
					is_error = 'Error en Parámetros de Existencia'
					RETURN 2
				ELSE
					li_bodevirtual 	= luo_existencia.bodvirtual
					li_bodzonal			= luo_existencia.bodzonal
					li_bodecomercial 	= luo_existencia.bodcomercial
				END IF	
							
				ll_docrel = dw_2.Object.mfge_numero[1]
					
//				IF li_categoria = 101 THEN
//					li_bodevirtual 	= luo_existencia.bodcomercial				
//				END IF	
				
				IF luo_existencia.Mesproceso > dw_2.Object.mfge_fecmov[1] THEN
					Message.DoubleParm = -1
					
					ll_docrel 	= dw_2.Object.mfge_numero[1]
					ld_fecha		= dw_2.Object.mfge_fecmov[1]
				
					IF luo_existencia.maximo_porfecha(3,il_packing,ll_docrel,1,ld_fecha,True,sqlexi) THEN
						ll_numero = luo_existencia.numero
					END IF
					
					IF isnull(ll_numero) THEN
						ll_numero = 0
					END IF	
							
					IF luo_existencia.bodega_zonal(il_packing,sqlexi,True) THEN
						ls_correozonal = luo_existencia.correozonal
					END IF	
					
					ls_correo = luo_existencia.correo
					
					IF ll_numero = 0 THEN
						ls_texto	 = "No es posible actualizar movto. en Existencia, por modificación anterior al mes de proceso(No presenta movimiento en bodegas)"
					ELSE	
						ls_texto	 = "No es posible actualizar movto. en Existencia Nº "+String(ll_numero)+"; por modificación anterior al mes de proceso"
					END IF
					
					ls_asunto = "Modifica Recepción fruta granel, Movto. Nº "+String(ll_docrel)
					//li_devcorreo =  Send_mail("smtp.rioblanco.cl","<frutaGranel@rioblanco.cl>","<"+ls_correo+">","","",ls_asunto,ls_texto,"",ls_error)
//					
//					IF (li_devcorreo<0) THEN
//						messagebox("Error No" + string(li_devcorreo),sErrorMsg)
//					END IF
					li_retorno = 2
					is_error = ls_texto
					RETURN 1
				END IF	
				
				luo_existencia.existeregistro(ll_docrel,il_packing,1,3,True,sqlexi,dw_2.Object.mfge_fecmov[1])
				
				IF luo_existencia.count = 0 THEN
					IF Not luo_existencia.correlativobode(3,il_packing,li_bodzonal,True,sqlexi) THEN
						Message.DoubleParm = -1
						li_retorno = 2
						is_error = 'Error en Tabla Correlativo Bodega'
						RETURN 2
					ELSE
						ll_numero = luo_existencia.numero
					END IF	
							
					IF isnull(ll_numero) THEN
						ll_numnuevoini = Long(String(il_packing)+''+'0001')
						ll_numnuevofin = Long(String(il_packing)+''+'9999')

						INSERT INTO dba.correlbode(bode_codigo,mden_tipdoc,cobo_bodadm,cobo_inicia,cobo_final,cobo_actual)
						VALUES(:il_packing,3,:li_bodzonal,:ll_numnuevoini,:ll_numnuevofin,:ll_numnuevoini)
						USING sqlexi;
						
						IF sqlexi.SQLCode = -1 THEN
							F_ErrorBaseDatos(sqlexi,"Correlbode")
							Message.DoubleParm = -1
							sqlexi.AutoCommit	=	ib_AutoCommit
							li_retorno = 2
							is_error = 'Problema con Correlativos de bodega'
							RETURN 2
						END IF
						ll_numero = ll_numnuevoini - 1
					END IF	
					ll_numero = ll_numero + 1
					
					ls_productor = String(dw_4.Object.prod_codigo[1],'000000')
							
					IF Not luo_existencia.Existecliente(ls_productor,True,sqlexi) THEN
						Message.DoubleParm = -1
						ls_productor = luo_existencia.prod
						li_retorno = 2
						is_error = 'Error en Tabla Cliente en existencia, es Posible que Cliente no Exista '+ls_productor
						RETURN 2
					END IF
					
					IF luo_existencia.prod = '' OR isnull(luo_existencia.prod) THEN
						ls_productor = luo_existencia.prdgen
					END IF	
					
					dw_exiencab.Object.mden_tipdoc[1] = 3
					dw_exiencab.Object.mden_numero[1] = ll_numero 
					dw_exiencab.Object.tpdo_codigo[1] = 3
					dw_exiencab.Object.mden_fecmov[1] = dw_2.Object.mfge_fecmov[1]
					dw_exiencab.Object.tpmv_tipomv[1] = 2
					dw_exiencab.Object.tpmv_codigo[1] = 16
					dw_exiencab.Object.mden_tipana[1] = 3
					dw_exiencab.Object.mden_estaci[1] = gstr_us.computador
					dw_exiencab.Object.mden_fecdig[1] = Date(Today())
					dw_exiencab.Object.mden_hordig[1] = Time(Now())
					
					//IF li_categoria <> 101 THEN
						dw_exiencab.Object.bode_codigo[1] = il_packing
					//ELSE
					//	dw_exiencab.Object.bode_codigo[1] = luo_existencia.bodcomercial
					//END IF 
					
					dw_exiencab.Object.mden_bodest[1] = li_bodzonal
					dw_exiencab.Object.clpr_rut[1]	 = ls_productor
					dw_exiencab.Object.mden_gdprod[1] = dw_4.Object.meen_guisii[1]
					dw_exiencab.Object.mden_docrel[1] = dw_2.Object.mfge_numero[1]
					dw_exiencab.Object.mden_fecdre[1] = dw_2.Object.mfge_fecmov[1]
					dw_exiencab.Object.mden_observ[1] = 'Recepción Envases Fruta Granel'
					dw_exiencab.Object.mden_estado[1] = 1
					dw_exiencab.Object.mden_pcopda[1] = 1
				ELSE
					luo_existencia.numero_actual(ll_docrel,il_packing,1,3,True,sqlexi,dw_2.Object.mfge_fecmov[1])
					ll_numero = luo_existencia.numeroactual
				END IF									
			END IF							
									
			li_enva_codigo = dw_envases_comer.Object.enva_codigo[ll_filaenva]
			li_enva_tipoen = dw_envases_comer.Object.enva_tipoen[ll_filaenva]
			ls_calidad		= dw_envases_comer.Object.cale_calida[ll_filaenva]
			ll_cantidad		= dw_envases_comer.Object.cantidad[ll_filaenva]
													
			ll_fila	=	dw_exideta.InsertRow(0)
									
			IF Not iuo_calicosechero.Existe(li_enva_tipoen,li_enva_codigo,ls_calidad,True,sqlca) THEN
				Message.DoubleParm = -1
				li_retorno = 2
				is_error = 'Problema con envases'
				RETURN 2
			ELSE
				ls_item_codigo = iuo_calicosechero.item 
			END IF	
			
			IF ls_item_codigo = '' OR IsNull(ls_item_codigo) THEN
				ls_item_codigo = luo_existencia.itgene
			END IF
							
			dw_exideta.Object.mden_tipdoc[ll_fila] = 3
			dw_exideta.Object.mden_numero[ll_fila] = ll_numero
			ll_secuencia2 ++
//			IF li_categoria = 101 THEN
//				dw_exideta.Object.mdde_secuen[ll_fila] = ll_secuencia2 
//			ELSE
				dw_exideta.Object.mdde_secuen[ll_fila] = li_secuencia 
//			END IF
			dw_exideta.Object.tpmv_tipomv[ll_fila] = 2 
			dw_exideta.Object.tpmv_codigo[ll_fila] = 16 
			dw_exideta.Object.item_codigo[ll_fila] = ls_item_codigo
			dw_exideta.Object.mdde_identi[ll_fila] = ''
			dw_exideta.Object.mdde_fecmov[ll_fila] = dw_2.Object.mfge_fecmov[1]
			dw_exideta.Object.bode_codigo[ll_fila] = il_packing
			dw_exideta.Object.mdde_cantid[ll_fila] = ll_cantidad
						
			li_secuencia = li_secuencia + 1
				
		NEXT		
		IF dw_exiencab.Rowcount() > 0 OR li_secuencia > 1 THEN
			lb_AutoCommit		=	sqlexi.AutoCommit
			sqlexi.AutoCommit	=	False
		
			IF dw_exiencab.Update(True, False) = 1 THEN
				IF dw_exideta.Update(True, False) = 1 THEN
					Commit;
					
					IF sqlexi.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlexi, This.Title)
						
						RollBack;
						li_retorno = 2
					//	Messagebox("Existencia","Grabación de Datos NO se realizó")
					ELSE
						lb_Retorno	=	True
						
						dw_exiencab.ResetUpdate()
						dw_exideta.ResetUpdate()
					//	Messagebox("Existencia","Grabación de Datos realizada Satisfactoriamente")
						li_retorno = 1
					END IF
				ELSE
					F_ErrorBaseDatos(sqlexi, This.Title)
					
					RollBack;
				//	Messagebox("Existencia","Grabación de Datos NO se realizó")
					li_retorno = 2
				END IF
			ELSE
				F_ErrorBaseDatos(sqlexi, This.Title)
				
				RollBack;
				//Messagebox("Existencia","Grabación de Datos NO se realizó")
				li_retorno = 2
			END IF
				
			dw_exiencab.Reset()
			dw_exideta.Reset()
			
		END IF
	NEXT	
	
	pb_grabar.Enabled = False
	dw_exiencab.Reset()
	dw_exideta.Reset()
	sqlexi.AutoCommit	=	lb_AutoCommit
	
	Return li_retorno

END IF


end event

event anulamovto();Long 		ll_numero, respuesta, ll_nueva, ll_fila, ll_filote, ll_filaenva
Boolean lb_AutoCommit
Integer	li_bodevirtual, li_bodzonal, li_bodecomercial, li_cliente, li_planta, li_tipomov, &
		ll_recepcion, li_lote, li_categoria 	
date	ld_fecha		
String	ls_productor

IF ib_Conectadoexistencia = TRUE THEN
	
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
	//	sqlexi.AutoCommit	=	ib_AutoCommit
		RETURN 
	ELSE
		li_bodevirtual 	= luo_existencia.bodvirtual
		li_bodzonal			= luo_existencia.bodzonal
		li_bodecomercial 	= luo_existencia.bodcomercial
	END IF	
		
	IF luo_existencia.Mesproceso > dw_2.Object.mfge_fecmov[1] THEN
		Message.DoubleParm = -1
		RETURN 
	END IF	
	
	dw_envases_comer.SetTransObject(sqlca)
	dw_lotescategoria.SetTransObject(sqlca)
	
	dw_lotescategoria.GetChild("vari_codigo",idwc_Variedad)
	idwc_Variedad.SetTransObject(SQLCA)
	idwc_Variedad.Retrieve(dw_2.Object.espe_codigo[1])
	
	li_cliente 		= dw_2.Object.clie_codigo[1]
	li_planta 		= dw_2.Object.plde_codigo[1] 
	li_tipomov		= dw_2.Object.tpmv_codigo[1]
	ll_recepcion 	= dw_2.Object.mfge_numero[1]

	dw_lotescategoria.Retrieve(li_planta,li_tipomov,ll_recepcion,li_cliente)
	
	ls_productor = String(dw_4.Object.prod_codigo[1],'000000')
							
	IF Not luo_existencia.Existecliente(ls_productor,True,sqlexi) THEN
		Message.DoubleParm = -1
		RETURN 
		ls_productor = luo_existencia.prod
	END IF
	
	IF luo_existencia.prod = '' OR isnull(luo_existencia.prod) THEN
		ls_productor = luo_existencia.prdgen
	END IF	
	
	IF isnull(dw_3.Object.lote_prdpak[1]) THEN
					
		IF NOT packingproductor(long(ls_productor)) THEN
			il_packing = dw_3.Object.lote_prdpak[1]
			
			IF luo_existencia.bodega_zonal(li_bodzonal,sqlexi,True) THEN
				
			END IF	
			
		END IF	
	ELSE
		il_packing = dw_3.Object.lote_prdpak[1]	
	END IF	
	
	luo_existencia.bodega_administradora(il_packing,True, sqlexi)	
	
	IF luo_existencia.bodeAdmi <> luo_existencia.bodzonal THEN
						
		SELECT isnull(plde_conpro,0)
		INTO :il_coneccion 
		FROM dbo.plantadesp 
		WHERE plde_codigo = :il_packing;
		
		IF il_coneccion = 0 THEN
			il_coneccion = 95
		END IF	
	ELSE
		SELECT 	cone_codigo
		INTO   	:il_coneccion
		FROM dbo.clientesprod
		WHERE clie_codigo = :li_Cliente;
	END IF
	
	FOR ll_filote = 1 TO dw_3.RowCount()
		
		li_lote			= dw_3.Object.lote_codigo[ll_filote]
		li_categoria 	= dw_3.Object.cate_codigo[ll_filote]
		
		dw_envases_comer.Retrieve(li_planta,li_lote,ll_recepcion,li_tipomov,li_cliente)
		
		FOR ll_filaenva = 1 TO dw_envases_comer.RowCount()
		
//			IF li_categoria = 101 THEN
//				li_bodevirtual 	= luo_existencia.bodcomercial				
//			END IF
				
			il_numeroenva = dw_2.Object.mfge_numero[1]
			ld_fecha			= dw_2.Object.mfge_fecmov[1]
			
			IF NOT luo_existencia.maximo_porfecha(1,li_bodzonal,il_numeroenva,1,ld_fecha,True,sqlexi) THEN
				Message.DoubleParm = -1
				Return
			ELSE
				ll_numero = luo_existencia.numero
			END IF
					
			IF NOT isnull(ll_numero) THEN
				//Return
						
			IF NOT luo_existencia.actualizaexistencia(2,1,li_bodzonal,ll_numero,il_numeroenva,True,sqlexi) THEN
				Message.DoubleParm = -1
				Return
			END IF	
			
			dw_exidetaborra.Retrieve(1,ll_numero)
			
			FOR ll_fila = 1 TO dw_exidetaborra.RowCount()
				ll_nueva = dw_exismovtodetanulos.InsertRow(0)
				
				dw_exismovtodetanulos.Object.mden_tipdoc[ll_nueva] = dw_exidetaborra.Object.mden_tipdoc[ll_fila]   
				dw_exismovtodetanulos.Object.mden_numero[ll_nueva] = dw_exidetaborra.Object.mden_numero[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_secuen[ll_nueva] = dw_exidetaborra.Object.mdde_secuen[ll_fila]  
				dw_exismovtodetanulos.Object.tpmv_tipomv[ll_nueva] = dw_exidetaborra.Object.tpmv_tipomv[ll_fila]  
				dw_exismovtodetanulos.Object.tpmv_codigo[ll_nueva] = dw_exidetaborra.Object.tpmv_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.item_codigo[ll_nueva] = dw_exidetaborra.Object.item_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.item_armado[ll_nueva] = dw_exidetaborra.Object.item_armado[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_identi[ll_nueva] = dw_exidetaborra.Object.mdde_identi[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_fecmov[ll_nueva] = dw_exidetaborra.Object.mdde_fecmov[ll_fila]  
				dw_exismovtodetanulos.Object.bode_codigo[ll_nueva] = dw_exidetaborra.Object.bode_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.ocen_numero[ll_nueva] = dw_exidetaborra.Object.mdde_ocompr[ll_fila]  
				dw_exismovtodetanulos.Object.ccon_codigo[ll_nueva] = dw_exidetaborra.Object.ccon_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.exic_codigo[ll_nueva] = dw_exidetaborra.Object.exic_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_cantid[ll_nueva] = dw_exidetaborra.Object.mdde_cantid[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_canarm[ll_nueva] = dw_exidetaborra.Object.mdde_canarm[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_prunmo[ll_nueva] = dw_exidetaborra.Object.mdde_prunmo[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_totmor[ll_nueva] = dw_exidetaborra.Object.mdde_totmor[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_totpes[ll_nueva] = dw_exidetaborra.Object.mdde_totpes[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_facpes[ll_nueva] = dw_exidetaborra.Object.mdde_facpes[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_facmor[ll_nueva] = dw_exidetaborra.Object.mdde_facmor[ll_fila]  
		
			NEXT
			
			DELETE FROM dbo.exismovtodeta 
				WHERE	mden_numero = :ll_numero
				AND	mden_tipdoc = 1
				AND	bode_codigo = :li_bodzonal
				USING sqlexi;
			
			IF sqlexi.SQLCode = -1 THEN
				F_ErrorBaseDatos(sqlexi,"exismovtodeta")
				Message.DoubleParm = -1
				sqlexi.AutoCommit	=	ib_AutoCommit
				RETURN 
			END IF
			
			IF dw_exismovtodetanulos.Rowcount() > 0 THEN
				lb_AutoCommit		=	sqlexi.AutoCommit
				sqlexi.AutoCommit	=	False
				
				IF dw_exismovtodetanulos.Update(True, False) = 1 THEN
					
					dw_exismovtodetanulos.ResetUpdate()
		
				ELSE
					F_ErrorBaseDatos(sqlexi, This.Title)
						
					RollBack;
					Return
				END IF
				
				sqlexi.AutoCommit	=	lb_AutoCommit
			
			END IF
		
		end if
		NEXT	
	NEXT
	//DISCONNECT USING sqlexi;
	//ib_Conectadoexistencia = False
END IF

end event

event anulamovto2();Long 		ll_numero, respuesta, ll_nueva, ll_fila, ll_filote, ll_filaenva
Boolean lb_AutoCommit
Integer	li_bodevirtual, li_bodzonal, li_bodecomercial, li_cliente, li_planta, li_tipomov, &
		ll_recepcion, li_lote, li_categoria 	
Date	ld_fecha		

IF ib_Conectadoexistencia = TRUE THEN
	
	uo_existencia				luo_existencia
	luo_existencia			=	Create uo_existencia
	
	IF Not luo_existencia.paramexistencia(True, sqlexi) THEN
		Message.DoubleParm = -1
	//	sqlexi.AutoCommit	=	ib_AutoCommit
		RETURN 
	ELSE
		li_bodevirtual 	= luo_existencia.bodvirtual
		li_bodzonal			= luo_existencia.bodzonal
		li_bodecomercial 	= luo_existencia.bodcomercial
	END IF	
		
	IF luo_existencia.Mesproceso > dw_2.Object.mfge_fecmov[1] THEN
		Message.DoubleParm = -1
		RETURN 
	END IF	
	
	dw_envases_comer.SetTransObject(sqlca)
	dw_lotescategoria.SetTransObject(sqlca)
	
	dw_lotescategoria.GetChild("vari_codigo",idwc_Variedad)
	idwc_Variedad.SetTransObject(SQLCA)
	idwc_Variedad.Retrieve(dw_2.Object.espe_codigo[1])
	
	li_cliente 		= dw_2.Object.clie_codigo[1]
	li_planta 		= dw_2.Object.plde_codigo[1] 
	li_tipomov		= dw_2.Object.tpmv_codigo[1]
	ll_recepcion 	= dw_2.Object.mfge_numero[1]

	dw_lotescategoria.Retrieve(li_planta,li_tipomov,ll_recepcion,li_cliente)
	
	FOR ll_filote = 1 TO dw_3.RowCount()
		
		li_lote			= dw_3.Object.lote_codigo[ll_filote]
		li_categoria 	= dw_3.Object.cate_codigo[ll_filote]
		
		dw_envases_comer.Retrieve(li_planta,li_lote,ll_recepcion,li_tipomov,li_cliente)
		
		FOR ll_filaenva = 1 TO dw_envases_comer.RowCount()
		
//			IF li_categoria = 101 THEN
//				li_bodevirtual 	= luo_existencia.bodcomercial				
//			END IF
				
			il_numeroenva = dw_2.Object.mfge_numero[1]
			ld_fecha			= dw_2.Object.mfge_fecmov[1]
			
			IF NOT luo_existencia.maximo_porfecha(3,il_packing,il_numeroenva,1,ld_fecha,True,sqlexi) THEN
				Message.DoubleParm = -1
				Return
			ELSE
				ll_numero = luo_existencia.numero
			END IF
					
			IF NOT isnull(ll_numero) THEN
				//Return
						
			IF NOT luo_existencia.actualizaexistencia(2,3,il_packing,ll_numero,il_numeroenva,True,sqlexi) THEN
				Message.DoubleParm = -1
				Return
			END IF	
			
			dw_exidetaborra.Retrieve(3,ll_numero)
			
			FOR ll_fila = 1 TO dw_exidetaborra.RowCount()
				ll_nueva = dw_exismovtodetanulos.InsertRow(0)
				
				dw_exismovtodetanulos.Object.mden_tipdoc[ll_nueva] = dw_exidetaborra.Object.mden_tipdoc[ll_fila]   
				dw_exismovtodetanulos.Object.mden_numero[ll_nueva] = dw_exidetaborra.Object.mden_numero[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_secuen[ll_nueva] = dw_exidetaborra.Object.mdde_secuen[ll_fila]  
				dw_exismovtodetanulos.Object.tpmv_tipomv[ll_nueva] = dw_exidetaborra.Object.tpmv_tipomv[ll_fila]  
				dw_exismovtodetanulos.Object.tpmv_codigo[ll_nueva] = dw_exidetaborra.Object.tpmv_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.item_codigo[ll_nueva] = dw_exidetaborra.Object.item_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.item_armado[ll_nueva] = dw_exidetaborra.Object.item_armado[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_identi[ll_nueva] = dw_exidetaborra.Object.mdde_identi[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_fecmov[ll_nueva] = dw_exidetaborra.Object.mdde_fecmov[ll_fila]  
				dw_exismovtodetanulos.Object.bode_codigo[ll_nueva] = dw_exidetaborra.Object.bode_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.ocen_numero[ll_nueva] = dw_exidetaborra.Object.mdde_ocompr[ll_fila]  
				dw_exismovtodetanulos.Object.ccon_codigo[ll_nueva] = dw_exidetaborra.Object.ccon_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.exic_codigo[ll_nueva] = dw_exidetaborra.Object.exic_codigo[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_cantid[ll_nueva] = dw_exidetaborra.Object.mdde_cantid[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_canarm[ll_nueva] = dw_exidetaborra.Object.mdde_canarm[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_prunmo[ll_nueva] = dw_exidetaborra.Object.mdde_prunmo[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_totmor[ll_nueva] = dw_exidetaborra.Object.mdde_totmor[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_totpes[ll_nueva] = dw_exidetaborra.Object.mdde_totpes[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_facpes[ll_nueva] = dw_exidetaborra.Object.mdde_facpes[ll_fila]  
				dw_exismovtodetanulos.Object.mdde_facmor[ll_nueva] = dw_exidetaborra.Object.mdde_facmor[ll_fila]  
		
			NEXT
			
			DELETE FROM dbo.exismovtodeta 
				WHERE	mden_numero = :ll_numero
				AND	mden_tipdoc = 3
				AND	bode_codigo = :il_packing
				USING sqlexi;
			
			IF sqlexi.SQLCode = -1 THEN
				F_ErrorBaseDatos(sqlexi,"exismovtodeta")
				Message.DoubleParm = -1
				sqlexi.AutoCommit	=	ib_AutoCommit
				RETURN 
			END IF
			
			IF dw_exismovtodetanulos.Rowcount() > 0 THEN
				lb_AutoCommit		=	sqlexi.AutoCommit
				sqlexi.AutoCommit	=	False
				
				IF dw_exismovtodetanulos.Update(True, False) = 1 THEN
					
					dw_exismovtodetanulos.ResetUpdate()
		
				ELSE
					F_ErrorBaseDatos(sqlexi, This.Title)
						
					RollBack;
					Return
				END IF
				
				sqlexi.AutoCommit	=	lb_AutoCommit
			
			END IF
		

		end if
		NEXT	
	NEXT
	//DISCONNECT USING sqlexi;
	//ib_Conectadoexistencia = False
END IF






























//Long 		ll_numero, respuesta, ll_fila, ll_nueva
//Boolean 	lb_AutoCommit
//Integer	li_bodecomercial
//
//IF ib_Conectadoexistencia = TRUE THEN
//	
//	SELECT DISTINCT expa_pacfri, expa_bodega, expa_pakcom
//	INTO :gi_bodvirtual, :gi_bodzonal, :li_bodecomercial
//	FROM dbo.existeparam
//	USING sqlexi;
//	
//	IF sqlexi.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlexi,"existeparam")
//		Message.DoubleParm = -1
//		sqlexi.AutoCommit	=	ib_AutoCommit
//		Return
//	END IF
//	
//	SELECT max(mden_numero)
//	INTO :ll_numero
//	FROM dbo.exismovtoenca
//	WHERE mden_tipdoc = 1
//	AND bode_codigo = :gi_bodvirtual
//	AND mden_bodest = :gi_bodzonal
//	AND mden_docrel = :il_numeroenva
//	USING sqlexi;
//	
//	IF sqlexi.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlexi,"exismovtoenca")
//		Message.DoubleParm = -1
//		sqlexi.AutoCommit	=	ib_AutoCommit
//		Return
//	END IF
//	
//	IF isnull(ll_numero) THEN
//		Return
//	END IF	
//	
//	UPDATE dbo.exismovtoenca SET
//	mden_estado = 2
//	WHERE mden_tipdoc = 1
//	AND bode_codigo = :gi_bodvirtual
//	AND mden_bodest = :gi_bodzonal
//	AND mden_numero = :ll_numero
//	AND mden_docrel = :il_numeroenva
//	USING sqlexi;
//	
//	IF sqlexi.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlexi,"exismovtoenca")
//		Message.DoubleParm = -1
//		sqlexi.AutoCommit	=	ib_AutoCommit
//		RETURN 
//	END IF
//	
//	dw_exidetaborra.Retrieve(1,ll_numero)
//	
//	FOR ll_fila = 1 TO dw_exidetaborra.RowCount()
//		ll_nueva = dw_exismovtodetanulos.InsertRow(0)
//		
//		dw_exismovtodetanulos.Object.mden_tipdoc[ll_nueva] = dw_exidetaborra.Object.mden_tipdoc[ll_fila]   
//		dw_exismovtodetanulos.Object.mden_numero[ll_nueva] = dw_exidetaborra.Object.mden_numero[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_secuen[ll_nueva] = dw_exidetaborra.Object.mdde_secuen[ll_fila]  
//		dw_exismovtodetanulos.Object.tpmv_tipomv[ll_nueva] = dw_exidetaborra.Object.tpmv_tipomv[ll_fila]  
//		dw_exismovtodetanulos.Object.tpmv_codigo[ll_nueva] = dw_exidetaborra.Object.tpmv_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.item_codigo[ll_nueva] = dw_exidetaborra.Object.item_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.item_armado[ll_nueva] = dw_exidetaborra.Object.item_armado[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_identi[ll_nueva] = dw_exidetaborra.Object.mdde_identi[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_fecmov[ll_nueva] = dw_exidetaborra.Object.mdde_fecmov[ll_fila]  
//		dw_exismovtodetanulos.Object.bode_codigo[ll_nueva] = dw_exidetaborra.Object.bode_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.ocen_numero[ll_nueva] = dw_exidetaborra.Object.mdde_ocompr[ll_fila]  
//		dw_exismovtodetanulos.Object.ccon_codigo[ll_nueva] = dw_exidetaborra.Object.ccon_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.exic_codigo[ll_nueva] = dw_exidetaborra.Object.exic_codigo[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_cantid[ll_nueva] = dw_exidetaborra.Object.mdde_cantid[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_canarm[ll_nueva] = dw_exidetaborra.Object.mdde_canarm[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_prunmo[ll_nueva] = dw_exidetaborra.Object.mdde_prunmo[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_totmor[ll_nueva] = dw_exidetaborra.Object.mdde_totmor[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_totpes[ll_nueva] = dw_exidetaborra.Object.mdde_totpes[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_facpes[ll_nueva] = dw_exidetaborra.Object.mdde_facpes[ll_fila]  
//		dw_exismovtodetanulos.Object.mdde_facmor[ll_nueva] = dw_exidetaborra.Object.mdde_facmor[ll_fila]  
//		//dw_exismovtodetanulos.Object.mdde_gratri[ll_nueva] = dw_exidetaborra.Object.mdde_codbar[ll_fila]  
////		dw_exismovtodetanulos.Object.mdde_atrco1[ll_nueva] = dw_exidetaborra.Object.mdde_codba1[ll_fila]  
////		dw_exismovtodetanulos.Object.mdde_atrco2[ll_nueva] = dw_exidetaborra.Object.mdde_codba2[ll_fila]  
////		dw_exismovtodetanulos.Object.mdde_atrco3[ll_nueva] = dw_exidetaborra.Object.mdde_codba3[ll_fila]  
//	NEXT	
//	
//	DELETE FROM dbo.exismovtodeta 
//	WHERE	mden_tipdoc = 1 
//	AND	mden_numero = :ll_numero
//	AND	bode_codigo = :gi_bodvirtual
//	USING sqlexi;
//	
//	IF sqlexi.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlexi,"exismovtodeta")
//		Message.DoubleParm = -1
//		sqlexi.AutoCommit	=	ib_AutoCommit
//		RETURN 
//	END IF
//	
//	IF dw_exismovtodetanulos.Rowcount() > 0 THEN
//		lb_AutoCommit		=	sqlexi.AutoCommit
//		sqlexi.AutoCommit	=	False
//		
//		IF dw_exismovtodetanulos.Update(True, False) = 1 THEN
//			
//			dw_exismovtodetanulos.ResetUpdate()
//
//		ELSE
//			F_ErrorBaseDatos(sqlexi, This.Title)
//				
//			RollBack;
//			Return
//		END IF
//		
//		sqlexi.AutoCommit	=	lb_AutoCommit
//	
//	END IF
//	
//	dw_exidetaborra.Reset()
//	dw_exismovtodetanulos.Reset()
//	DISCONNECT USING sqlexi;
//	
//END IF
//
//
end event

public subroutine determina_productoresenvase (integer ai_tipomovto);str_Productores_Envases	lstr_Prod_Enva
DataWindow	ldw_envase
Long			ll_Fila, ll_GuiaSII, ll_Fila_Lote
Integer		li_Secuencia
Long			ll_Productor,ll_ProdAnt , ll_Numero

//Inicializa la Estructura de Instancia
wstr_Prod_Enva	= lstr_Prod_Enva

IF ai_TipoMovto = 41 THEN
	ldw_envase	=	dw_5
ELSE
	ldw_envase	=	dw_7
END IF

FOR ll_Fila	=	1 TO ldw_Envase.RowCount()
	ll_Productor	=	ldw_Envase.Object.prod_codigo[ll_Fila]
	ll_Numero		=	ldw_Envase.Object.meen_numero[ll_Fila]
	
	IF ll_Productor <> ll_ProdAnt THEN
		ll_ProdAnt		=	ll_Productor
		li_Secuencia 	++
		
		IF ai_TipoMovto = 41 THEN
			ll_Fila_Lote	=	dw_3.Find("prod_codigo = "+String(ll_Productor),1,dw_3.RowCount())
			
			IF ll_Fila_Lote > 0 THEN
				ll_GuiaSII	=	dw_3.Object.lote_guisii[ll_Fila_Lote]
			ELSE
				ll_GuiaSII	=	0
			END IF
		END IF
		
		wstr_Prod_Enva.Productor[li_Secuencia]	=	ll_Productor
		wstr_Prod_Enva.GuiaSII[li_Secuencia]	=	ll_GuiaSII
		wstr_Prod_Enva.TipoMovto[li_Secuencia]	=	ai_TipoMovto
		wstr_Prod_Enva.Numero[li_Secuencia]		=	ll_Numero
	END IF
NEXT

RETURN
end subroutine

public subroutine productoreslotes (ref string productores[]);Long		ll_Fila
Integer	li_Secuencia
String		ls_productor, ls_ProdAnt, ls_Nula[]

Productores	=	ls_Nula

FOR ll_Fila = 1 TO dw_3.RowCount()
	ls_Productor	=	dw_3.Object.prod_rut[ll_Fila]
	
	IF ls_Productor <> ls_ProdAnt THEN
		li_Secuencia 					++
		Productores[li_Secuencia]	=	ls_Productor
		ls_ProdAnt						=	ls_Productor
	END IF
NEXT

RETURN
end subroutine

protected function integer wf_modifica ();IF dw_3.AcceptText() = -1 THEN RETURN -1
IF dw_5.AcceptText() = -1 THEN RETURN -1
IF dw_7.AcceptText() = -1 THEN RETURN -1
IF dw_2.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0
IF (dw_3.ModifiedCount() + dw_3.DeletedCount()) > 0 THEN RETURN 0
IF (dw_5.ModifiedCount() + dw_5.DeletedCount()) > 0 THEN RETURN 0
IF (dw_7.ModifiedCount() + dw_7.DeletedCount()) > 0 THEN RETURN 0
IF dw_2.ModifiedCount() > 0 THEN RETURN 0

RETURN 1
end function

public function boolean chequea_envasesproductor ();Boolean	lb_Retorno	=	True

RETURN lb_Retorno
end function

public function boolean otrosmovtosdetalle ();Long	ll_Fila, ll_Fila_Busca

Boolean	lb_Retorno
Integer	li_Camara, li_PlantaLote, li_EspecieLote, li_NumeroLote, &
			li_TipoEnvase, li_Envase, li_Saldo, li_Cantidad

FOR ll_fila = 1 TO dw_6.RowCount()
	
	li_PlantaLote	=	dw_6.Object.lote_pltcod[ll_Fila]
	li_EspecieLote	=	dw_6.Object.lote_espcod[ll_Fila]
	li_NumeroLote	=	dw_6.Object.lote_codigo[ll_Fila]
	li_TipoEnvase	=	dw_6.Object.enva_tipoen[ll_Fila]
	li_Envase		=	dw_6.Object.enva_codigo[ll_Fila]
	li_Cantidad		=	dw_6.Object.lotd_totbul[ll_Fila]

	ll_Fila_Busca	=	dw_3.Find("lote_pltcod = "+String(li_PlantaLote)+" and "+&
										 "lote_espcod = "+String(li_EspecieLote)+" and "+&
										 "lote_codigo = "+String(li_NumeroLote),1,dw_3.RowCount())
	IF ll_Fila_Busca > 0 THEN
		li_Camara		=	dw_3.Object.cama_codigo[ll_Fila_Busca]
	END IF

	SELECT	IsNull(Sum(caex_canbul), 0)
	  INTO	:li_Saldo
	  FROM	dbo.spro_camaraexistefg
	 WHERE	plde_codigo	=	:li_PlantaLote
		AND	cama_codigo	=	:li_Camara
		AND	lote_pltcod	=	:li_PlantaLote
		AND	lote_espcod	=	:li_EspecieLote
		AND	lote_codigo	=	:li_NumeroLote
		AND 	enva_tipoen	=	:li_TipoEnvase
		AND	enva_codigo	=	:li_Envase;

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura tabla de Existencia Frigorífico")
		
		lb_Retorno	=	True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "No existe Lote en Cámara especificada.~r~r" + &
						"Ingrese o seleccione otros antecedentes.")
		
		lb_Retorno	=	True
		EXIT
	ELSEIF li_Cantidad <> li_Saldo THEN
		MessageBox("Atención", "Lote "+String(li_plantalote,'0000')+String(li_EspecieLote,'00')+&
						String(li_NumeroLote,'0000')+" ha sufrido cambios.  Saldo en Cámara es ~r" + &
						"distinto a la cantidad de bultos recepcionados.~r~r" + &
						"No se puede modificar antecedentes.")
		
		lb_Retorno	=	True
		EXIT
	END IF
NEXT

RETURN lb_Retorno
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_Retorno
Long		ll_Fila

If Not dw_2.uf_check_required(0) Then RETURN False

If Not dw_1.uf_validate(0) Then RETURN False

If NOT Borrando Then
	If dw_2.GetItemStatus(1, 0, Primary!) = NewModIfied! Then
		dw_2.SetItem(1,"mfge_usuari",gstr_us.nombre)
		dw_2.SetItem(1,"mfge_comput",gstr_us.computador)
		dw_2.SetItem(1,"mfge_horacr",F_FechaHora())
	ElseIf dw_2.GetItemStatus(1, 0, Primary!) = DataModIfied! Then
		dw_2.SetItem(1,"mfge_usumod",gstr_us.nombre)
		dw_2.SetItem(1,"mfge_commod",gstr_us.computador)
		dw_2.SetItem(1,"mfge_horact",F_FechaHora())
		
		If dw_3.DeletedCount() > 0 Then Borrando = True
		If dw_6.DeletedCount() > 0 Then Borrando = True
	End If
End If

If Borrando Then
	If dw_pdf.Update(True, False) = 1 Then
		If Eliminacion_Packing() Then
			If dw_1.Update(True, False) = 1 Then 								//Detalle
				If dw_6.Update(True, False) = 1 Then							//Detalle de Lotes
					If dw_spro_bins.Update(True,False) = 1 Then
						If dw_3.Update(True, False) = 1 Then					//Lotes
							If dw_9.Update(True, False) = 1 Then				//Detalle de Pesaje
								If dw_2.Update(True, False) = 1 Then			//Encabezado
									If dw_5.Update(True,False) = 1 Then			//Envases Recepcionados
										If dw_7.Update(True,False) = 1 Then		//Envases Retirados
											If dw_4.Update(True,False) = 1 Then	//Encabezados Envases
												If dw_desverd.Update(True,False) = 1 Then
													Commit;
										
													If sqlca.SQLCode <> 0 Then
														F_ErrorBaseDatos(sqlca, This.Title)
														RollBack;
													Else
														lb_Retorno	=	True
													
														dw_6.ResetUpdate()
														dw_7.ResetUpdate()
														dw_5.ResetUpdate()
														dw_4.ResetUpdate()
														dw_3.ResetUpdate()
														dw_2.ResetUpdate()
														dw_1.ResetUpdate()
														dw_9.ResetUpdate()
														dw_spro_bins.ResetUpdate()
														dw_desverd.ResetUpdate()
													End If
												Else
													F_ErrorBaseDatos(sqlca, This.Title)
													RollBack;
												End If
											Else
												F_ErrorBaseDatos(sqlca, This.Title)
												RollBack;
											End If
										Else
											F_ErrorBaseDatos(sqlca, This.Title)
											RollBack;
										End If
									Else
										F_ErrorBaseDatos(sqlca, This.Title)
										RollBack;
									End If
								Else
									F_ErrorBaseDatos(sqlca, This.Title)
									RollBack;
								End If
							Else
								F_ErrorBaseDatos(sqlca, This.Title)
								RollBack;
							End If
						Else
							F_ErrorBaseDatos(sqlca, This.Title)
							RollBack;
						End If
					Else
						F_ErrorBaseDatos(sqlca, This.Title)
						RollBack;
					End If
				Else
					F_ErrorBaseDatos(sqlca, This.Title)
					RollBack;
				End If
			Else
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			End If
		Else
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	End If
Else
	If dw_2.Update(True, False) = 1 Then		 					//Encabezado
		If dw_3.Update(True, False) = 1 Then						//Lotes
			If dw_6.Update(True, False) = 1 Then					//Detalle de Lotes
 				If dw_1.Update(True, False) = 1 Then				//Detalle
					If dw_4.Update(True,False) = 1 Then				//Encabezado Envases
						If dw_5.Update(True,False) = 1 Then			//Envases Recepcionados
							If dw_7.Update(True,False) = 1 Then		//Envases Retirados
								If dw_9.Update(True,False) = 1 Then	//Detalle Pesaje
									If dw_spro_bins.Update(True,False) = 1 Then
										If dw_desverd.Update(True,False) = 1 Then
											If Actual_ultimo_lote() Then
												Commit;
										
												If sqlca.SQLCode <> 0 Then
													F_ErrorBaseDatos(sqlca, This.Title)
												Else
													lb_Retorno	=	True
												
													dw_6.ResetUpdate()
													dw_7.ResetUpdate()
													dw_5.ResetUpdate()
													dw_4.ResetUpdate()
													dw_3.ResetUpdate()
													dw_2.ResetUpdate()
													dw_1.ResetUpdate()
													dw_9.ResetUpdate()
													dw_spro_bins.ResetUpdate()
													dw_desverd.ResetUpdate()
													
												End If
											Else
												F_ErrorBaseDatos(sqlca, This.Title)
												RollBack;
											End If
										Else
											F_ErrorBaseDatos(sqlca, This.Title)
											RollBack;
										End If
									Else
										F_ErrorBaseDatos(sqlca, This.Title)
										RollBack;
									End If
								Else
									F_ErrorBaseDatos(sqlca, This.Title)
									RollBack;
								End If
							Else
								F_ErrorBaseDatos(sqlca, This.Title)
								RollBack;
							End If
						Else
							F_ErrorBaseDatos(sqlca, This.Title)
							RollBack;
						End If
					Else
						F_ErrorBaseDatos(sqlca, This.Title)
						RollBack;
					End If
				Else
					F_ErrorBaseDatos(sqlca, This.Title)
					RollBack;
				End If
			Else
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			End If
		Else
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	End If
End If

sqlca.AutoCommit	=	ib_AutoCommit

RETURN lb_Retorno
end function

public subroutine captura_totales ();Long			ll_Total_Bultos, ll_Fila
Decimal{3}	ld_Total_PesoEnv_Ent,ld_Total_PesoEnv_Sal, ld_Total_Neto,&
				ld_Total_KBE, ld_Total_KBS, ld_KBE_Camion, ld_KBE_Carro,&
				ld_KBS_Camion, ld_KBS_Carro, ld_TaraCamion, ld_TaraCarro

dw_2.AcceptText()
dw_3.AcceptText()
dw_5.AcceptText()
dw_7.AcceptText()

ll_Fila	=	dw_3.RowCount()

IF ll_Fila > 0 THEN
	dw_3.GroupCalc()
	IF dw_3.Object.total_bultos[ll_fila] > 10000 THEN
		Messagebox("Error de Consistencia","El total de bultos supera lo permitido")
		RETURN
	ELSE
		ll_Total_Bultos		=	dw_3.GetItemNumber(ll_Fila,"total_bultos")
	END IF
END IF


ll_Fila	=	dw_5.RowCount()

IF ll_Fila > 0 THEN
	dw_5.GroupCalc()
	IF dw_5.Object.total_pesoenv[ll_Fila] >= 100000 THEN
		Messagebox("Error de Consistencia","El peso de los envase supera lo permitido")
		RETURN
	ELSE
		ld_Total_PesoEnv_Ent	=	dw_5.Object.total_pesoenv[ll_Fila]
	END IF
	
	IF dw_5.Object.total_envases[ll_Fila] >= 10000 THEN
		Messagebox("Error de Consistencia","El total de bultos supera lo permitido")
		RETURN
	END IF
END IF

ll_Fila	=	dw_7.RowCount()

IF ll_Fila > 0 THEN
	dw_7.GroupCalc()
	ld_Total_PesoEnv_Sal	=	dw_7.Object.total_pesoenv[ll_Fila]
END IF

//Determina Peso Neto del Camión
IF ib_Salida THEN
	ld_KBE_Camion	=	dw_2.Object.refg_tkbent[1]
	ld_KBE_Carro	=	dw_2.Object.refg_tkbenc[1]
	
	IF Isnull(ld_KBE_Carro) THEN ld_KBE_Carro = 0
	
	ld_Total_KBE	=	ld_KBE_Camion + ld_KBE_Carro
	
	ld_KBS_Camion	=	dw_2.Object.refg_tkbsal[1]
	ld_KBS_Carro	=	dw_2.Object.refg_tkbsac[1]
	ld_TaraCamion	=	iuo_Camion.TaraCamion
	ld_TaraCarro	=	iuo_Camion.TaraCarro
	
	IF Isnull(ld_TaraCamion) THEN ld_TaraCamion = 0
	IF Isnull(ld_TaraCarro) THEN ld_TaraCarro = 0
	
	IF ld_KBS_Camion + ld_KBS_Carro > 0 AND &
		ld_KBS_Camion + ld_KBS_Carro < ld_Total_PesoEnv_Sal THEN
		MessageBox("Atención","Kilos de Salida no pueden ser menor a los Envases de Salida")
		RETURN
	ELSE
		ld_Total_KBS	=	ld_KBS_Camion + ld_KBS_Carro - ld_Total_PesoEnv_Sal
	END IF
		
	ld_Total_Neto					=	ld_Total_KBE - ld_Total_PesoEnv_Ent - ld_Total_KBS
	
	dw_2.Object.refg_tkensa[1]	=	ld_Total_PesoEnv_Sal
	dw_2.Object.mfge_tpneto[1]	=	ld_Total_Neto

END IF

dw_2.Object.refg_tkenen[1]	=	ld_Total_PesoEnv_Ent	

IF ib_Destare THEN
	Destare(True)
END IF

RETURN
end subroutine

public function boolean actual_ultimo_lote ();Long 		ll_Filas, ll_Fila, ll_Lote
Integer 	li_Lote_espcod, li_planta
String 	ls_nombrepc

li_Planta	=	Integer(istr_mant.Argumento[1])
ll_Filas	=	dw_3.RowCount()
ll_Fila	=	0
ll_Lote	=	0

DO WHILE ll_Fila < ll_Filas
	ll_Fila = dw_3.GetNextModified(ll_fila, Primary!)
	IF ll_Fila > 0 AND dw_3.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		IF ll_Lote < dw_3.Object.lote_codigo[ll_Fila] THEN
			ll_lote 			=	dw_3.Object.lote_codigo[ll_Fila]
			li_Lote_espcod	=	dw_3.Object.lote_espcod[ll_Fila]
		END IF
	END IF	
LOOP

IF ll_Lote  > 0 THEN
	ls_nombrepc	=	gstr_us.computador
	
	UPDATE	dbo.spro_lotescorrelequipo
		SET	loco_ultcor	=	:ll_Lote
	 WHERE	plde_codigo		=	:li_Planta
		AND	espe_codigo		=	:li_Lote_espcod
		AND 	Upper(equi_nombre) 	=	Upper(:ls_nombrepc)
	 USING	sqlca;

	IF SQLCA.SQLCode = -1 THEN
		F_ErrorBaseDatos(SQLCA,"Actualización último Lote")
		Message.DoubleParm = -1
		SQLCA.AutoCommit	=	ib_AutoCommit
		RETURN FALSE
	END IF
END IF

RETURN TRUE
end function

public function boolean verificalote (integer ai_planta, integer ai_especie, long al_lote);Integer li_planta, li_cantidad

li_planta = dw_2.Object.plde_codigo[1]

SELECT count(*) INTO :li_Cantidad
  FROM dbo.spro_movtofrutagrandeta
 WHERE plde_codigo = :li_planta
   AND lote_pltcod = :ai_planta
   AND lote_espcod = :ai_especie
   AND lote_codigo = :al_lote
   AND tpmv_codigo <> 1 ;

IF isnull(li_cantidad) THEN li_cantidad = 0

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Recepción Fruta Granel")
	RETURN FALSE
	
ELSEIF li_cantidad > 1 THEN
	MessageBox("Atención","El Lote "+String(al_lote,'0000')+ " posee otros movimientos. No se puede eliminar.")
	RETURN FALSE
	
END IF

RETURN TRUE
end function

public function boolean verificabultos (integer ai_planta, integer ai_loteplanta, integer ai_especie, integer ai_lotecod, integer ai_bultos);Integer  li_Resultado

sqlca.AutoCommit = TRUE

DECLARE ExistenciaBultos PROCEDURE FOR dbo.Fgran_BultosExistencia
		@Planta     =: ai_Planta,
		@LotePld 	=:	ai_loteplanta,
		@Especie    =: ai_especie,
		@LoteCod		=: ai_lotecod,
		@Bultos		=: ai_bultos
USING SQLCA;
					
EXECUTE ExistenciaBultos;
Fetch ExistenciaBultos into :li_Resultado;
CLOSE ExistenciaBultos;

sqlca.AutoCommit = FALSE
	
IF li_Resultado = 1 THEN
	RETURN TRUE		
ELSE
	RETURN FALSE
END IF
		


end function

public subroutine destare (boolean ab_actualiza);Long			ll_Fila, li_filas, ll_Fila_lote
Integer		li_Especie, li_TipoEnvase, li_Envase, li_TipoAnt, li_EnvaAnt,&
				li_TotBultos, li_Secuencia, li_Lote, li_LoteAnt, li_BultosLote, li_Cliente
Boolean		lb_FueraRango
Date			ld_Fechamovto
Decimal{3}	ld_TotalNeto, ld_PesoEstandar[], ld_PesoMinimo[], ld_PesoMaximo[],&
				ld_TotalEstandar, ld_PesoDistrib, ld_LoteDistrib, ld_NetoLoteEnvase,&
				ld_TotalDistrib, ld_Remanente
Double		ld_Factor
/*
INICIO Cambio hecho en coquimbo
*/
Decimal Ld_entradaa, Ld_entradab, Ld_salidaa, Ld_salidab, ld_enven, ld_envsan, ld_TaraBultos

ld_entradaa						=	dw_2.Object.refg_tkbent[1] 
ld_entradab						=	dw_2.Object.refg_tkbenc[1]
ld_salidaa						= 	dw_2.Object.refg_tkbsal[1]
ld_salidab						=	dw_2.Object.refg_tkbsac[1] 
ld_enven							=	dw_2.Object.refg_tkenen[1]
ld_envsan						=	dw_2.Object.refg_tkensa[1]
dw_2.Object.mfge_tpneto[1] = 	(Ld_entradaa - Ld_salidaa) + ( Ld_entradab - Ld_salidab) - ( ld_enven + ld_envsan)

li_Especie						=	dw_2.Object.espe_codigo[1]
ld_FechaMovto					=	dw_2.Object.mfge_fecmov[1]
ld_TotalNeto					=	dw_2.Object.mfge_tpneto[1]
li_Cliente							=	dw_2.Object.clie_codigo[1]

dw_3.SetSort("lote_pltcod A, lote_espcod A, lote_codigo A")
dw_3.Sort()

dw_6.SetSort("lote_pltcod A, lote_espcod A, lote_codigo A, enva_tipoen A, enva_codigo A")
dw_6.Sort()

FOR ll_Fila = 1 TO dw_6.RowCount()
	li_TipoEnvase	=	dw_6.Object.enva_tipoen[ll_Fila]
	li_Envase		=	dw_6.Object.enva_codigo[ll_Fila]
	
	If li_TipoEnvase <> li_TipoAnt OR li_Envase <> li_EnvaAnt Then
		If iuo_PesoEstanEspe.Existe(li_Especie, li_TipoEnvase, li_Envase, ld_FechaMovto, True, SQLCA) Then
			li_Secuencia ++
			ld_PesoEstandar[li_Secuencia]	=	iuo_PesoEstanEspe.PesoDistrib
			ld_PesoMinimo[li_Secuencia]	=	iuo_PesoEstanEspe.PesoMinimo
			ld_PesoMaximo[li_Secuencia]	=	iuo_PesoEstanEspe.PesoMaximo
		Else
			RETURN
		End If
		li_TipoAnt	=	li_TipoEnvase
		li_EnvaAnt	=	li_Envase
	End If
	
	li_TotBultos		=	dw_6.Object.lotd_totbul[ll_Fila]
	ld_TotalEstandar	=	ld_TotalEstandar + (li_TotBultos*ld_PesoEstandar[li_Secuencia])
	
NEXT

If IsNull(ld_TotalEstandar) OR ld_TotalEstandar = 0 Then
	Return
End If

ld_Factor		=	ld_TotalNeto / ld_TotalEstandar
li_Secuencia	=	0
li_TipoAnt		=	0
li_EnvaAnt		=	0

FOR ll_Fila = 1 TO dw_6.RowCount()
	li_Lote			=	dw_6.Object.lote_codigo[ll_Fila]
	li_TipoEnvase	=	dw_6.Object.enva_tipoen[ll_Fila]
	li_Envase		=	dw_6.Object.enva_codigo[ll_Fila]
	
	If li_Lote <> li_LoteAnt Then
		If li_LoteAnt > 0 Then
			ld_PesoDistrib = Round(ld_LoteDistrib / li_BultosLote,3)
			If ab_actualiza Then
				dw_3.Object.lote_kilpro[ll_Fila_lote]	=	ld_PesoDistrib
				dw_3.Object.lote_totnet[ll_Fila_lote]	=	ld_LoteDistrib
			End If
			If lb_FueraRango Then
				dw_3.Object.font[ll_Fila_lote]	=	1
			Else
				dw_3.Object.font[ll_Fila_lote]	=	0
			End If
		End If
		
		ll_Fila_lote ++
		li_BultosLote	=	dw_3.Object.lote_totbul[ll_Fila_lote]
		ld_LoteDistrib	=	0
		li_LoteAnt		=	li_Lote
		lb_FueraRango	=	False
		
	End If
	
	If li_TipoEnvase <> li_TipoAnt OR li_Envase <> li_EnvaAnt Then
		li_Secuencia ++
		li_TipoAnt	=	li_TipoEnvase
		li_EnvaAnt	=	li_Envase
	End If
	
	li_TotBultos			=	dw_6.Object.lotd_totbul[ll_Fila]
	ld_PesoDistrib		=	Round(ld_PesoEstandar[li_Secuencia] * ld_Factor,ii_kildec)
	ld_NetoLoteEnvase	=	Round(ld_PesoDistrib * li_TotBultos,ii_kildec)
	
	If ld_PesoDistrib < ld_PesoMinimo[li_Secuencia] OR ld_PesoDistrib > ld_PesoMaximo[li_Secuencia] Then
		dw_6.Object.font[ll_Fila]	=	1
		lb_FueraRango				=	True
	Else
		dw_6.Object.font[ll_Fila]	=	0
	End If
	
	If ab_actualiza Then
		dw_6.Object.lotd_kilpro[ll_Fila]		=	ld_PesoDistrib
		dw_6.Object.lotd_totnet[ll_Fila]	=	ld_NetoLoteEnvase
	End If
	
	ld_LoteDistrib		=	ld_LoteDistrib 	+ ld_NetoLoteEnvase
	ld_TotalDistrib	=	ld_TotalDistrib 	+ ld_NetoLoteEnvase
	
NEXT

If li_LoteAnt > 0 Then
	ld_PesoDistrib = Round(ld_LoteDistrib / li_BultosLote, ii_kildec)
	
	If ab_actualiza Then
		dw_3.Object.lote_kilpro[ll_Fila_lote]	=	ld_PesoDistrib
		dw_3.Object.lote_totnet[ll_Fila_lote]	=	ld_LoteDistrib
	End If


	If lb_FueraRango Then
		dw_3.Object.font[ll_Fila_lote]	=	1
	Else
		dw_3.Object.font[ll_Fila_lote]	=	0
	End If
End If

If ab_actualiza Then
	ld_Remanente = ld_TotalDistrib - ld_TotalNeto
	
	If ld_Remanente <> 0 Then
		ld_NetoLoteEnvase									=	dw_6.Object.lotd_totnet[dw_6.RowCount()]
		dw_6.Object.lotd_totnet[dw_6.RowCount()]	=	ld_NetoLoteEnvase - ld_Remanente
		
		ld_NetoLoteEnvase									=	dw_3.Object.lote_totnet[dw_3.RowCount()]
		dw_3.Object.lote_totnet[dw_3.RowCount()]	=	ld_NetoLoteEnvase - ld_Remanente
	End If
	
	ib_Destare				=	True
	pb_grabar.Enabled	=	True

End If

end subroutine

public subroutine habilitasalida ();dw_2.Object.refg_tkbsal.Protect				=	0
dw_2.Object.refg_tkbsac.Protect				=	0

dw_2.Object.refg_tkbsal.BackGround.Color	=	0
dw_2.Object.refg_tkbsac.BackGround.Color	=	0

dw_2.Object.refg_tkbsal.BackGround.Color	=	RGB(255,255,255)
dw_2.Object.refg_tkbsac.BackGround.Color	=	RGB(255,255,255)
tab_1.tp_3.Enabled								=	True

dw_2.Object.refg_tkbent.Protect				=	1
dw_2.Object.refg_tkbenc.Protect				=	1
dw_2.Object.mfge_totbul.Protect				=	1

dw_2.Object.refg_tkbent.Color					=	RGB(255,255,255)
dw_2.Object.refg_tkbenc.Color					=	RGB(255,255,255)
dw_2.Object.mfge_totbul.Color					=	RGB(255,255,255)

dw_2.Object.refg_tkbent.BackGround.Color	=	553648127
dw_2.Object.refg_tkbenc.BackGround.Color	=	553648127
dw_2.Object.mfge_totbul.BackGround.Color	=	553648127

ib_Salida											=	True
dw_2.Object.destare.Text					=	'Destare'
pb_grabar.Enabled							=	False
pb_imprimir.Enabled							=	False

captura_totales()
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.mfge_numero.Protect				=	0
	dw_2.Object.mfge_numero.Color					=	0
	dw_2.Object.mfge_numero.BackGround.Color	=	RGB(255,255,255)
	
	dw_2.Object.espe_codigo.Protect				=	1
	dw_2.Object.tran_codigo.Protect				=	1
	dw_2.Object.cami_patent.Protect				=	1
	dw_2.Object.cami_patcar.Protect				=	1
	dw_2.Object.mfge_rutcho.Protect				=	1
	dw_2.Object.mfge_chofer.Protect				=	1
	dw_2.Object.mfge_totbul.Protect				=	1
	dw_2.Object.refg_tkbent.Protect				=	1
	dw_2.Object.refg_tkbenc.Protect				=	1
	dw_2.Object.mfge_fecmov.Protect				=	1
	dw_2.Object.refg_horaen.Protect				=	1
	dw_2.Object.mfge_totbul.Protect				=	1
	dw_2.Object.refg_tkbsal.Protect				=	1
	dw_2.Object.refg_tkbsac.Protect				=	1
	dw_2.Object.refg_horasa.Protect				=	1
	dw_2.Object.refg_fecsal.Protect				=	1
	
	dw_2.Object.espe_codigo.Color		=	Rgb(255,255,255)
	dw_2.Object.tran_codigo.Color			=	Rgb(255,255,255)
	dw_2.Object.cami_patent.Color		=	Rgb(255,255,255)
	dw_2.Object.cami_patcar.Color		=	Rgb(255,255,255)
	dw_2.Object.mfge_rutcho.Color		=	Rgb(255,255,255)
	dw_2.Object.mfge_chofer.Color		=	Rgb(255,255,255)
	dw_2.Object.mfge_totbul.Color			=	Rgb(255,255,255)
	dw_2.Object.refg_tkbent.Color			=	Rgb(255,255,255)
	dw_2.Object.refg_tkbenc.Color			=	Rgb(255,255,255)
	dw_2.Object.mfge_fecmov.Color		=	Rgb(255,255,255)
	dw_2.Object.refg_horaen.Color		=	Rgb(255,255,255)
	dw_2.Object.mfge_totbul.Color			=	Rgb(255,255,255)
	dw_2.Object.refg_tkbsal.Color			=	Rgb(255,255,255)
	dw_2.Object.refg_tkbsac.Color			=	Rgb(255,255,255)
	dw_2.Object.refg_horasa.Color			=	Rgb(255,255,255)
	dw_2.Object.refg_fecsal.Color			=	Rgb(255,255,255)

	dw_2.Object.espe_codigo.BackGround.Color	=	553648127
	dw_2.Object.tran_codigo.BackGround.Color		=	553648127
	dw_2.Object.cami_patent.BackGround.Color		=	553648127
	dw_2.Object.cami_patcar.BackGround.Color		=	553648127
	dw_2.Object.mfge_rutcho.BackGround.Color	=	553648127
	dw_2.Object.mfge_chofer.BackGround.Color	=	553648127
	dw_2.Object.mfge_totbul.BackGround.Color		=	553648127
	dw_2.Object.refg_tkbent.BackGround.Color		=	553648127
	dw_2.Object.refg_tkbenc.BackGround.Color		=	553648127
	dw_2.Object.mfge_fecmov.BackGround.Color	=	553648127
	dw_2.Object.refg_horaen.BackGround.Color		=	553648127
	dw_2.Object.mfge_totbul.BackGround.Color		=	553648127
	dw_2.Object.refg_tkbsal.BackGround.Color		=	553648127
	dw_2.Object.refg_tkbsac.BackGround.Color		=	553648127
	dw_2.Object.refg_horasa.BackGround.Color		=	553648127
	dw_2.Object.refg_fecsal.BackGround.Color		=	553648127
	tab_1.tp_3.Enabled								=	False
ELSE
	dw_2.Object.mfge_numero.Protect			=	1
	dw_2.Object.espe_codigo.Protect				=	1
	dw_2.Object.tran_codigo.Protect				=	1
	dw_2.Object.cami_patent.Protect				=	1
	dw_2.Object.cami_patcar.Protect				=	1
	dw_2.Object.mfge_rutcho.Protect				=	1
	dw_2.Object.mfge_chofer.Protect				=	1
	
	dw_2.Object.mfge_numero.Color				=	RGB(255,255,255)	
	dw_2.Object.espe_codigo.Color				=	RGB(255,255,255)	
	dw_2.Object.tran_codigo.Color					=	RGB(255,255,255)	
	dw_2.Object.cami_patent.Color				=	RGB(255,255,255)	
	dw_2.Object.cami_patcar.Color				=	RGB(255,255,255)	
	dw_2.Object.mfge_rutcho.Color				=	RGB(255,255,255)	
	dw_2.Object.mfge_chofer.Color				=	RGB(255,255,255)	
	
	dw_2.Object.mfge_numero.BackGround.Color	=	553648127
	dw_2.Object.espe_codigo.BackGround.Color	=	553648127
	dw_2.Object.tran_codigo.BackGround.Color		=	553648127
	dw_2.Object.cami_patent.BackGround.Color		=	553648127
	dw_2.Object.cami_patcar.BackGround.Color		=	553648127
	dw_2.Object.mfge_rutcho.BackGround.Color	=	553648127
	dw_2.Object.mfge_chofer.BackGround.Color	=	553648127
	
	dw_2.Object.refg_tkbent.Protect				=	0
	dw_2.Object.refg_tkbenc.Protect				=	0
	dw_2.Object.refg_tkbsal.Protect				=	0
	dw_2.Object.refg_tkbsac.Protect				=	0
	dw_2.Object.mfge_totbul.Protect				=	0
	
	dw_2.Object.refg_tkbent.Color	=	0
	dw_2.Object.refg_tkbenc.Color	=	0
	dw_2.Object.refg_tkbsal.Color	=	0
	dw_2.Object.refg_tkbsac.Color	=	0
	dw_2.Object.mfge_totbul.Color	=	0
	
	dw_2.Object.refg_tkbent.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.refg_tkbenc.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.refg_tkbsal.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.refg_tkbsac.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.mfge_totbul.BackGround.Color	=	RGB(255,255,255)	

	tab_1.tp_3.Enabled								=	True
END IF
end subroutine

public function boolean existerecepcion (integer ai_planta, integer ai_tipomovto, integer al_numero, integer ai_cliente);Long		ll_Numero
Integer	li_estado
Boolean	lb_Retorno = True

SELECT	mfge_numero, mfge_estmov
	INTO	:ll_Numero, :li_estado
	FROM	dbo.spro_movtofrutagranenca
	WHERE	plde_codigo	=	:ai_Planta
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	mfge_numero	=	:al_Numero 
	AND   clie_codigo =  :ai_cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Recepción Fruta Granel")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
	IF li_estado <> 3 THEN
		MessageBox("Atención","Número de Correlativo no ha sido destarado. Ingrese Otro.")
		lb_Retorno	=	False
		
	ELSE
		istr_mant.Argumento[3]	=	String(al_Numero)
		This.TriggerEvent("ue_recuperadatos")
	END IF
	
ELSE
	MessageBox("Atención","Número de Correlativo No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
	
END IF

RETURN lb_Retorno
end function

public function boolean noexistecliente (integer cliente);String ls_nombre

SELECT clie_nombre
  INTO :ls_nombre
  FROM dbo.clientesprod
 WHERE clie_codigo =:cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClientesProd")
	Return True
	
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	Return True
	
END IF

Return False
end function

public subroutine actualizakiloslote ();Long			ll_Fila, ll_Filas
Integer		li_Planta_Lote, li_Especie_Lote, li_Numero_Lote
Decimal{3}	ld_Kilos_Neto
Boolean		lb_Actualiza

uo_duchacontrol	luo_duchacontrol

luo_duchacontrol	=	Create uo_duchacontrol

ll_Filas	=	dw_3.RowCount()

ib_AutoCommit		=	SQLCA.AutoCommit
SQLCA.AutoCommit	=	False

FOR	ll_Fila	=	1 TO ll_Filas
	IF dw_3.Object.lote_ducha[ll_Fila]	=	1 THEN
		li_Planta_Lote		=	dw_3.Object.lote_pltcod[ll_Fila]
		li_Especie_Lote	=	dw_3.Object.lote_espcod[ll_Fila]
		li_Numero_Lote		=	dw_3.Object.lote_codigo[ll_Fila]
		ld_Kilos_Neto		=	dw_3.Object.lote_totnet[ll_Fila]
		
		IF luo_duchaControl.TieneDuchaLote(li_Planta_Lote, li_Especie_Lote, li_Numero_Lote, False, SQLCA) THEN
			IF luo_duchaControl.ActualizaKilosLote(li_Planta_Lote, li_Especie_Lote, li_Numero_Lote, ld_Kilos_Neto, SQLCA) THEN
				lb_actualiza	=	True
			END IF
		END IF
	END IF
NEXT

Destroy luo_duchacontrol

IF lb_Actualiza THEN
	Commit;
END IF

sqlca.AutoCommit	=	ib_AutoCommit

RETURN
end subroutine

public subroutine binsapalletfruta ();Integer li_filas_dw3, li_filas_dw2, li_bulto

dw_2.RowsMove(1, dw_2.RowCount(), Primary!, dw_2, 1, Delete!)

FOR li_filas_dw3 = 1 TO dw_3.RowCount()
	
	li_filas_dw2	=	dw_2.find('enva_tipoen = ' + String(dw_3.Object.enva_tipoen[li_filas_dw3]) + ' and ' +&
									    'enva_codigo = ' + String(dw_3.Object.enva_codigo[li_filas_dw3]), 1, dw_2.RowCount() )
										
	IF li_filas_dw2 < 1 THEN
		li_filas_dw2	=	dw_2.InsertRow(0)
		dw_2.Object.lote_pltcod[li_filas_dw2]	=	Integer(istr_mant.Argumento[1])
		dw_2.Object.lote_espcod[li_filas_dw2]	=	Integer(istr_mant.Argumento[5])
		dw_2.Object.lote_codigo[li_filas_dw2]	=	dw_3.Object.lote_codigo[li_filas_dw3]
		dw_2.Object.enva_tipoen[li_filas_dw2]	=	dw_3.Object.enva_tipoen[li_filas_dw3]
		dw_2.Object.enva_codigo[li_filas_dw2]	=	dw_3.Object.enva_codigo[li_filas_dw3]
	END IF
	
	li_bulto 											=	dw_2.Object.lotd_totbul[li_filas_dw2]
	IF IsNull(li_bulto) THEN li_bulto 			= 	0
	dw_2.Object.lotd_totbul[li_filas_dw2] 		=	li_bulto + 1
	
NEXT
end subroutine

public subroutine asignapeso ();Long 		ll_Fila
Decimal 	ld_pesos = 0

IF dw_9.RowCount() > 0 THEN
	FOR ll_Fila = 1 TO dw_9.RowCount()
		ld_pesos =	ld_pesos  + dw_9.Object.mfgp_pesore[ll_fila]
	NEXT
END IF

IF NOT gstr_paramplanta.bultobins THEN
	dw_2.Object.refg_tkbent[1]	 = ld_pesos
END IF
end subroutine

public function boolean procesopacking ();integer 	li_clie_codigo, li_i, li_hora, li_mins, li_mfge_numero, li_CuentaTP, li_CuentaVac, li_Secuen
date 		ld_date_actual

//-------variables para spro_ordenprocvacdeta
String 	ls_cale_calida
Decimal	ld_KilosRecep, ld_Tara, ld_NetoOriginal, ld_Neto, ld_KilosPromedio
time 		lt_hora_ivaciado, lt_hora_tvaciado

//-------variables para lotefrutagranel
integer 	li_vari_codigo, li_prod_codigo

//-------variables para lotefrutagrandeta
integer 	li_plde_codigo, li_lote_espcod, li_lote_codigo, li_enva_tipoen, &
		  	li_enva_codigo, li_lotd_totbul, li_lotd_totnet, li_lotd_kilpro
			  
//-------variables para orden de proceso 
integer 	li_orpr_tipoord, li_orpr_numero

//-------variables para programa de proceso 
integer 	li_ppre_numero, li_ppre_feccre

li_clie_codigo 		= 	dw_2.Object.clie_codigo[1]
li_mfge_numero 	= 	dw_2.Object.mfge_numero[1]

li_plde_codigo 		= 	dw_6.Object.lote_pltcod[1]
li_lote_espcod		= 	dw_6.Object.lote_espcod[1]
li_lote_codigo		= 	dw_6.Object.lote_codigo[1]
li_enva_tipoen		= 	dw_6.Object.enva_tipoen[1]
li_enva_codigo		= 	dw_6.Object.enva_codigo[1]
li_lotd_totbul			= 	dw_6.Object.lotd_totbul[1]
li_lotd_totnet		= 	dw_6.Object.lotd_totnet[1]
li_lotd_kilpro			= 	dw_6.Object.lotd_kilpro[1]

IF iuo_especie.existe(dw_2.object.espe_codigo[1],TRUE,SQLCA) THEN
	IF iuo_especie.kildec = 1 THEN
		ii_kildec = 2
	ELSE
		ii_kildec = 0
	END IF	
END IF

SELECT vari_codigo, 
		 prod_codigo, 
		 lote_totnet
  INTO :li_vari_codigo, 
  		 :li_prod_codigo, 
		 :ld_kilosRecep
  FROM dbo.spro_lotesfrutagranel as lote
 WHERE lote.lote_pltcod	= :li_plde_codigo
	AND lote.lote_espcod	= :li_lote_espcod
	AND lote.lote_codigo	= :li_lote_codigo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla LotesFrutaGranel")
	Return FALSE

ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Encabezado del lote no ha sido generado, se aborta el proceso. Ingrese Otro.")
	Return FALSE

END IF

Select orpr_fecpro Into :ld_date_actual
	From dbo.spro_ordenproceso
	Where clie_codigo 	=	:li_clie_codigo and
			 plde_codigo	=	:li_plde_codigo and
			 orpr_tipord 	=	4 					 and
		    orpr_numero 	=	:li_lote_codigo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla LotesFrutaGranel")
	Return FALSE

ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Encabezado del lote no ha sido generado, se aborta el proceso. Ingrese Otro.")
	Return FALSE

END IF
			 
If IsNull(ld_date_actual) Or ld_date_actual = Date('1900/01/01') Then ld_date_actual 	= 	DATE(STRING(TODAY(), "dd/mm/yyyy"))

li_hora 				= 	Hour(now())
li_mins 				= 	Minute(now())
lt_hora_ivaciado 	= 	time(li_hora, li_mins,0)
lt_hora_tvaciado 	= 	time(li_hora + 1, li_mins,0)

//*********************************** tablas de encabezados
//No es necesario actualizar tablas de encabezados
//*********************************** tablas de detalles
For Li_i = 1 to dw_6.RowCount()	
	li_hora 				= 	Hour(now())
	li_mins 				= 	Minute(now())
	lt_hora_ivaciado 	= 	time(li_hora, li_mins + li_i,0)
	lt_hora_tvaciado 	= 	time(li_hora + 1, li_mins + li_i,0)

	li_plde_codigo 	= 	dw_6.Object.lote_pltcod[li_i]
	li_lote_espcod		= 	dw_6.Object.lote_espcod[li_i]
	li_lote_codigo		= 	dw_6.Object.lote_codigo[li_i]
	li_enva_tipoen		= 	dw_6.Object.enva_tipoen[li_i]
	li_enva_codigo		= 	dw_6.Object.enva_codigo[li_i]
	li_lotd_totbul			= 	dw_6.Object.lotd_totbul[li_i]
	li_lotd_totnet		= 	Round(dw_6.Object.lotd_totnet[li_i],ii_kildec)
	li_lotd_kilpro			= 	Round(dw_6.Object.lotd_kilpro[li_i],ii_kildec)

	UPDATE dbo.spro_ordenproceso 
	   SET orpr_canbul 	=	:li_lotd_totbul
 	 WHERE clie_codigo 	=	:li_clie_codigo and
			 plde_codigo	=	:li_plde_codigo and
			 orpr_tipord 	=	4 					 and
		    orpr_numero 	=	:li_lote_codigo;
			 
			 
	Update dbo.spro_lotesfrutagrandeta set
			lotd_totbul	=	:li_lotd_totbul,
			lotd_totnet	=	:li_lotd_totnet,
			lotd_kilpro	=	:li_lotd_kilpro
	Where lote_pltcod = :li_plde_codigo
		And	lote_espcod = :li_lote_espcod
		And	lote_codigo = :li_lote_codigo
		And 	enva_tipoen = :li_enva_tipoen
		And enva_codigo = :li_enva_codigo;

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Actualización de Tabla spro_ordenproceso")
		Return FALSE

	ELSEIF sqlca.SQLCode <> 0 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE

	END IF

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Actualización de Tabla Detalle del Lote")
		Return FALSE

	ELSEIF sqlca.SQLCode <> 0 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE

	END IF

	IF li_clie_codigo = 81 THEN
		ls_cale_calida = "1"

	ELSE
		ls_cale_calida = "2"

	END IF
	
	Select IsNull(lote_secuen,0)  Into :li_CuentaTP
		From dbo.spro_terminoproceso  
	 WHERE plde_codigo	= 	:li_plde_codigo AND
			 tpmv_codigo	= 	21 				 AND 
			 tepr_numero	= 	:li_lote_codigo AND
			 tepr_secuen 	= 	1 					 AND
			 lote_pltcod 	= 	:li_plde_codigo AND
			 lote_espcod 	= 	:li_lote_espcod AND   
			 lote_codigo 	=	:li_lote_codigo AND
			 lote_secuen	=	:li_i 			 AND 
			 enva_tipoen	=	:li_enva_tipoen AND
			 enva_codigo	=	:li_enva_codigo AND
			 clie_codigo	=	:li_clie_codigo;
			 
	IF li_CuentaTP = 0 Then		 
		INSERT INTO dbo.spro_terminoproceso  
				( plde_codigo, tpmv_codigo, tepr_numero,   
				  tepr_secuen, lote_pltcod, lote_espcod,   
				  lote_codigo, lote_secuen, enva_tipoen,   
				  enva_codigo, clie_codigo, tepr_bultra,   
				  tepr_bulvac )  
		VALUES (:li_plde_codigo, 21, :li_lote_codigo,   
				  :li_i, :li_plde_codigo, :li_lote_espcod,   
				  :li_lote_codigo, :li_i, :li_enva_tipoen,   
				  :li_enva_codigo, :li_clie_codigo, :li_lotd_totbul,   
				  :li_lotd_totbul )  ;
				  
		IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_terminoproceso")
			Return FALSE
		ELSEIF sqlca.SQLCode = 100 THEN
			MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
			Return FALSE
		END IF
	Else

		UPDATE dbo.spro_terminoproceso  
			SET tepr_bultra	= 	:li_lotd_totbul,
				 tepr_bulvac 	=	:li_lotd_totbul
		 WHERE plde_codigo	= 	:li_plde_codigo AND
				 tpmv_codigo	= 	21 				 AND 
				 tepr_numero	= 	:li_lote_codigo AND
				 tepr_secuen 	= 	1 					 AND
				 lote_pltcod 	= 	:li_plde_codigo AND
				 lote_espcod 	= 	:li_lote_espcod AND   
				 lote_codigo 	=	:li_lote_codigo AND
				 lote_secuen	=	:li_i 			 AND 
				 enva_tipoen	=	:li_enva_tipoen AND
				 enva_codigo	=	:li_enva_codigo AND
				 clie_codigo	=	:li_clie_codigo;
	
		IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca, "Actualización de Tabla spro_terminoproceso")
			Return FALSE
	
		ELSEIF sqlca.SQLCode <> 0 AND NOT gstr_paramplanta.semipacking THEN
			MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
			Return FALSE
	
		END IF
	End If	

	UPDATE dbo.spro_movtofrutagrandeta 
		SET mfgd_bulent	=	:li_lotd_totbul,
			 mfgd_kgnent	=	:li_lotd_totnet 
	 WHERE plde_codigo	=	:li_plde_codigo AND
		    tpmv_codigo	=	21 				 AND
		    mfge_numero	=	:li_mfge_numero AND
		    clie_codigo	=	:li_clie_codigo AND
		    mfgd_secuen	=	:Li_i 			 AND
		    plde_coorde	=	:li_plde_codigo AND
		    cama_codigo	=	1 					 AND
		    lote_pltcod	=	:li_plde_codigo AND
		    lote_espcod	=	:li_lote_espcod AND
		    lote_codigo	=	:li_lote_codigo AND
		    enva_tipoen	=	:li_enva_tipoen AND
		    enva_codigo	=	:li_enva_codigo;

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Actualización de Tabla spro_movtofrutagrandeta")
		Return FALSE

	ELSEIF sqlca.SQLCode <> 0 AND NOT gstr_paramplanta.semipacking THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE

	END IF
	

	UPDATE dbo.spro_ordenprocdeta 
	   SET orpd_canbul	=	:li_lotd_totbul 
	 WHERE plde_codigo	=	:li_plde_codigo AND
		    orpr_tipord	=	4 					 AND
		    orpr_numero	=	:li_lote_codigo AND
		    clie_codigo	=	:li_clie_codigo AND
		    orpd_secuen	=	:Li_i 			 AND
		    lote_pltcod	=	:li_plde_codigo AND
		    lote_espcod	=	:li_lote_espcod AND
		    lote_codigo 	=	:li_lote_codigo ;

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Actuaclización de Tabla spro_ordenprocdeta")
		Return FALSE

	ELSEIF sqlca.SQLCode <> 0 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE

	END IF

	ld_NetoOriginal		=	Round(li_lotd_totnet,ii_kildec)//ld_KilosRecep
	ld_Neto				=	Round(ld_NetoOriginal,ii_kildec)
	ld_KilosPromedio	=	Round(ld_Neto / li_lotd_totbul,ii_kildec)
	ld_KilosRecep		=	Round(li_lotd_totnet/*ld_KilosRecep*/ + ( ld_tara * li_lotd_totbul ),ii_kildec)
	
	Select Count(*) into :li_CuentaVac
		From dbo.spro_ordenprocvacdeta 
		Where plde_codigo = :li_plde_codigo
			And orpr_tipord = 4
			And orpr_numero = :li_lote_codigo
			And lote_espcod = :li_lote_espcod
			And lote_pltcod =:li_plde_codigo
			And enva_tipoen = :li_enva_tipoen
			And enva_codigo = :li_enva_codigo
			And cale_calida = :ls_cale_calida;
			
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenprocvacdeta")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF
	
   If li_CuentaVac = 0 Then			
	
		INSERT INTO dbo.spro_ordenprocvacdeta  
			( plde_codigo, orpr_tipord, orpr_numero,    clie_codigo, opve_fecvac, opve_turno,   
			  opvd_horava, opvd_horate, lote_pltcod,   lote_espcod, lote_codigo, enva_tipoen,   
			  enva_codigo, cale_calida, opvd_canbul,  opvd_kilpro, opvd_kilori, opvd_pesobr, opvd_pesone)  
		VALUES ( :li_plde_codigo, 4, :li_lote_codigo,   :li_clie_codigo, :ld_date_actual, 1,   
		  :lt_hora_ivaciado, :lt_hora_tvaciado, :li_plde_codigo,  :li_lote_espcod, :li_lote_codigo, :li_enva_tipoen,   
		  :li_enva_codigo, :ls_cale_calida, :li_lotd_totbul, :ld_KilosPromedio, :ld_NetoOriginal, :ld_KilosRecep, :ld_Neto)  ;
		  
		IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenprocvacdeta")
			Return FALSE
		ELSEIF sqlca.SQLCode = 100 THEN
			MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
			Return FALSE
		END IF
	Else

		UPDATE dbo.spro_ordenprocvacdeta 
			SET opvd_canbul	=	:li_lotd_totbul,
				 opvd_kilpro 	= 	:ld_KilosPromedio,
				 opvd_kilori	=	:ld_NetoOriginal, 
				 opvd_pesobr	=	:ld_KilosRecep,
				 opvd_pesone	=	:ld_Neto 
		 WHERE plde_codigo	=	:li_plde_codigo AND
				 orpr_tipord	=	4 					 AND
				 orpr_numero	=	:li_lote_codigo AND
				 clie_codigo	=	:li_clie_codigo AND
				 lote_pltcod	=	:li_plde_codigo AND
				 lote_espcod	=	:li_lote_espcod AND
				 lote_codigo	=	:li_lote_codigo AND
				 enva_tipoen = :li_enva_tipoen And 
				 enva_codigo = :li_enva_codigo And 
				 cale_calida = :ls_cale_calida	 ;
	
		IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca, "Actualización de Tabla spro_ordenprocvacdeta")
			Return FALSE
	
		ELSEIF sqlca.SQLCode <> 0 AND NOT gstr_paramplanta.semipacking THEN
			MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
			Return FALSE
	
		END IF
	End If

  	UPDATE dbo.spro_movtofrutagranenca 
	   SET mfge_estmov = 2  
	 WHERE ( dbo.spro_movtofrutagranenca.plde_codigo = :li_plde_codigo ) AND  
	  		 ( dbo.spro_movtofrutagranenca.clie_codigo = :li_clie_codigo ) AND  
		    ( dbo.spro_movtofrutagranenca.tpmv_codigo = 21 ) 					AND  
		    ( dbo.spro_movtofrutagranenca.mfge_numero = :li_lote_codigo );

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutagranenca")
		Return FALSE

	ELSEIF sqlca.SQLCode = 100 AND NOT gstr_paramplanta.semipacking THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE

	END IF

	UPDATE dbo.spro_ordenproceso  
   	SET orpr_estado = 2  
	 WHERE ( dbo.spro_ordenproceso.plde_codigo = :li_plde_codigo ) AND  
  		    ( dbo.spro_ordenproceso.clie_codigo = :li_clie_codigo ) AND  
		    ( dbo.spro_ordenproceso.orpr_tipord = 4 ) 					AND  
          ( dbo.spro_ordenproceso.orpr_numero = :li_lote_codigo );

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenproceso")
		Return FALSE
		
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
		
	END IF
NEXT 

RETURN TRUE
end function

public function boolean conexionexistencia ();String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta

IF ib_ConectadoExistencia THEN
	DISCONNECT USING sqlexi;
END IF

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

SELECT cone_nomodb,cone_nomser,cone_nombas,
		cone_nodbms,cone_nomusu,cone_passwo  
 INTO :ls_nomodb,:ls_nomser,:ls_nombas,
		:ls_nodbms,:ls_Usuario,:ls_Password
 FROM dbo.prodconectividad   
WHERE cone_codigo = :il_coneccion;

sqlexi.ServerName	=	ls_nomser
sqlexi.DataBase	=	ls_nombas
sqlexi.Dbms			= 	ls_nodbms
sqlexi.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'
CONNECT USING sqlexi;
is_base = ls_nombas

IF sqlexi.SQLCode = 0 THEN
	ib_ConectadoExistencia	=	True
ELSE
	ib_ConectadoExistencia	=	False
END IF

RETURN ib_ConectadoExistencia

end function

public function boolean chequea_binscontraenvases ();Integer	li_fila, li_str, li_tipenv, li_codenv, li_cantid
Long		ll_prod[]
String	ls_calcos, ls_mensaje
boolean	lb_Respuesta = TRUE, lb_existe
str_envase	lstr_env[]
DataStore	lds_bins
uo_bins		luo_bins

luo_bins						=	Create uo_bins
lds_bins						=	Create DataStore
lds_bins.DataObject		=	dw_spro_bins.DataObject

dw_spro_bins.RowsCopy(1, dw_spro_bins.RowCount(), Primary!, lds_bins, 1, Primary!)
lds_bins.SetSort("enva_tipoen asc, enva_codigo asc, cale_calida asc")
lds_bins.Sort()

IF lds_bins.RowCount() 	= 	0 THEN RETURN lb_respuesta

li_str						=	1
li_fila						=	1
lstr_env[li_str].tipenv = 	lds_bins.Object.enva_tipoen[li_fila]
lstr_env[li_str].codenv = 	lds_bins.Object.enva_codigo[li_fila]
lstr_env[li_str].calcos = 	lds_bins.Object.cale_calida[li_fila]

FOR li_fila = 1 TO dw_3.RowCount()
	ll_prod[li_fila]		=	dw_3.Object.prod_codigo[li_fila]
NEXT

FOR li_fila = 1 TO lds_bins.RowCount()
	IF NOT luo_bins.Existe(lds_bins.Object.clie_codigo[li_fila],&
					 			  lds_bins.Object.plde_codigo[li_fila],&
							 	  lds_bins.Object.bins_numero[li_fila],sqlca,True) THEN
		RETURN FALSE
	END IF
	
	IF lstr_env[li_str].tipenv <> luo_bins.enva_tipoen OR &
		lstr_env[li_str].codenv <> luo_bins.enva_codigo OR &
		lstr_env[li_str].calcos <> luo_bins.cale_calida THEN
		
		li_str						=	li_str + 1
		lstr_env[li_str].tipenv = 	luo_bins.enva_tipoen
		lstr_env[li_str].codenv = 	luo_bins.enva_codigo
		lstr_env[li_str].calcos = 	luo_bins.cale_calida
		
		IF NOT gstr_paramplanta.palletdebins THEN
			lstr_env[li_str].cantid = 	1
		ELSE
			lstr_env[li_str].cantid =	lds_bins.Object.fgmb_canbul[li_fila]
		END IF
		
	ELSE
		
		IF NOT gstr_paramplanta.palletdebins THEN
			lstr_env[li_str].cantid =	lstr_env[li_str].cantid + 1
		ELSE
			lstr_env[li_str].cantid =	lstr_env[li_str].cantid + lds_bins.Object.fgmb_canbul[li_fila]
		END IF
		
	END IF

NEXT

lds_bins.SetSort("fgmb_tibapa asc")
lds_bins.Sort()

FOR li_fila = 1 TO lds_bins.RowCount()
	
	IF lds_bins.Object.fgmb_tibapa[li_fila] > 0 OR &
		Not IsNull(lds_bins.Object.fgmb_tibapa[li_fila]) THEN
		IF NOT luo_bins.Existe(lds_bins.Object.clie_codigo[li_fila],&
									  lds_bins.Object.plde_codigo[li_fila],&
									  lds_bins.Object.fgmb_tibapa[li_fila],sqlca,True) THEN
			RETURN FALSE
		END IF
		
		IF lstr_env[li_str].tipenv <> luo_bins.enva_tipoen OR &
			lstr_env[li_str].codenv <> luo_bins.enva_codigo OR &
			lstr_env[li_str].calcos <> luo_bins.cale_calida THEN
			
			li_str						=	li_str + 1
			lstr_env[li_str].tipenv = 	luo_bins.enva_tipoen
			lstr_env[li_str].codenv = 	luo_bins.enva_codigo
			lstr_env[li_str].calcos = 	luo_bins.cale_calida
			lstr_env[li_str].cantid = 	1
			
		ELSE
			lstr_env[li_str].cantid =	lstr_env[li_str].cantid + 1
			
		END IF
	END IF
NEXT


FOR li_fila = 1 TO dw_5.RowCount()
	
	li_tipenv	=	dw_5.Object.enva_tipoen[li_fila]
	li_codenv 	= 	dw_5.Object.enva_codigo[li_fila]
	ls_calcos 	= 	dw_5.Object.cale_calida[li_fila]
	li_cantid	=	dw_5.Object.fgme_cantid[li_fila]
	
	FOR li_str = 1 TO UpperBound(lstr_env)
		IF lstr_env[li_str].tipenv = li_tipenv AND &
			lstr_env[li_str].codenv = li_codenv AND &
			lstr_env[li_str].calcos = ls_calcos THEN
			
			lb_existe = TRUE
			IF lstr_env[li_str].cantid <> li_cantid THEN
				ls_mensaje = ls_mensaje + "* La cantidad de envases ("+String(li_tipenv)+")("+String(li_codenv)+")("+ls_calcos+") "
				ls_mensaje = ls_mensaje + "ingresada no corresponde con los bins ingresados con esas caracteristicas.~r~n"
			END IF			
		END IF
	NEXT
	IF NOT lb_existe THEN
		ls_mensaje = ls_mensaje + "* Los "+String(li_cantid)+" envases ingresados ("+String(li_tipenv)+")("+String(li_codenv)+")("+ls_calcos+") "
		ls_mensaje = ls_mensaje + "no corresponde a las caractisticas de ningún Bins ingresado.~r~n"
	END IF
	
	lb_existe = False
	FOR li_str = 1 TO UpperBound(ll_prod[])
		IF dw_5.Object.prod_codigo[li_fila]	=	ll_prod[li_str] THEN
			lb_existe = TRUE
		END IF
	NEXT
	IF NOT lb_existe THEN
		ls_mensaje = ls_mensaje + "* El productor ("+String(dw_5.Object.prod_codigo[li_fila])+") con envases ingresados ("+String(li_tipenv)+")("+String(li_codenv)+")("+ls_calcos+") "
		ls_mensaje = ls_mensaje + "no corresponde a el o los productores de los lotes recepcionados.~r~n"
	END IF
	
	lb_existe = False
NEXT

IF Len(ls_mensaje) > 0 THEN
	lb_respuesta = False
	MessageBox("Error de Consistencia", "Se han detectado los siguientes errores con los envases ingresados:~r~n" + ls_mensaje, StopSign!)
ELSE
	lb_respuesta = True
END IF
RETURN lb_Respuesta
end function

public subroutine codigoconexion ();Integer	li_cliente

li_Cliente	=	dw_2.Object.clie_codigo[1]
		
SELECT clie_conexi, cone_codigo
  INTO :il_conexiste, :il_coneccion
  FROM dbo.clientesprod
 WHERE clie_codigo = :li_Cliente;
end subroutine

public subroutine updatemovtogranpesa ();Long		ll_fila, ll_FindLote
Decimal	ld_kilpro
uo_bins	luo_bins
luo_bins	=	Create uo_bins

FOR ll_fila = 1 TO dw_9.RowCount()
	
	ll_FindLote	=	dw_3.Find("lote_codigo = " + String(dw_9.Object.lote_codigo[ll_fila]), 1, dw_3.RowCount())
	
	IF ll_FindLote	> 0 THEN
		
		iuo_bins.Existe(dw_2.Object.clie_codigo[1],&
							 dw_2.Object.plde_codigo[1],&
							 dw_9.Object.bins_numero[ll_fila], sqlca, TRUE)
		
		ld_kilpro	=	dw_3.Object.lote_kilpro[ll_FindLote]
		
		IF gstr_paramplanta.aplicaporc = 1 AND dw_2.Object.espe_codigo[1] = 21 THEN
			luo_bins.Existe(dw_2.Object.clie_codigo[1],&
								 dw_2.Object.plde_codigo[1],&
								 dw_9.Object.mfgp_tibapa[ll_fila], sqlca, TRUE)
		ELSE
			luo_bins.cale_pesoen	=	0
		END IF
		
		dw_9.Object.mfgp_pesore[ll_fila]	=	Dec(ld_kilpro + iuo_bins.cale_pesoen + (luo_bins.cale_pesoen / dw_3.Object.lote_totbul[ll_FindLote]))
		dw_9.Object.mfgp_valref[ll_fila]	=	Dec(dw_9.Object.mfgp_pesore[ll_fila] * dw_3.Object.lote_totbul[ll_FindLote])
		
	END IF
	
NEXT
end subroutine

public function boolean packingproductor (long al_productor);
SELECT prpk_bodega
  INTO :il_packing
  FROM dbo.prodpacking
 WHERE prod_codigo =:al_productor;

IF il_packing = 0 OR isnull(il_packing) THEN
	Return False
END IF

Return True
end function

public subroutine wf_productoreslotes (ref string productores[]);Long		ll_Fila
Integer	li_Secuencia
String	ls_productor, ls_ProdAnt, ls_Nula[]

Productores	=	ls_Nula

For ll_Fila = 1 To dw_3.RowCount()
	ls_Productor	=	String(dw_3.Object.prod_codigo[ll_Fila], '00000')

	If ls_Productor <> ls_ProdAnt Then
		li_Secuencia 					++
		Productores[li_Secuencia]	=	ls_Productor
		ls_ProdAnt						=	ls_Productor
	End If
Next

Return
end subroutine

public function boolean wf_grabaregistro ();Boolean	lb_Retorno = True
String		ls_Productores[], ls_Archivo
Long		ll_Fila, ll_New, ll_Maximo, ll_Secuencia, ll_Productor

wf_ProductoresLotes(ls_Productores)

ll_Maximo		= 	UpperBound(ls_Productores)
ll_Secuencia	=	iuo_HuertoPDF.Maximo(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1], dw_2.Object.tpmv_codigo[1], dw_2.Object.mfge_numero[1], &
						dw_2.Object.espe_codigo[1], dw_2.Object.mfge_fecmov[1], Sqlca) + 1

If ll_Maximo > 0 Then
	For ll_Fila = 1 To ll_Maximo
		ls_Archivo	=	'RecepcionHuerto' + String(dw_2.Object.mfge_numero[1], '00000000') + ls_Productores[ll_Fila] + '.pdf'
		ll_New = dw_pdf.insertRow(0)
		
		ll_Productor								=	Long(ls_Productores[ll_Fila])
		dw_pdf.Object.clie_codigo[ll_New]	=	dw_2.Object.clie_codigo[1]
		dw_pdf.Object.plde_codigo[ll_New]	=	dw_2.Object.plde_codigo[1]
		dw_pdf.Object.tpmv_codigo[ll_New]	=	dw_2.Object.tpmv_codigo[1]
		dw_pdf.Object.mfge_numero[ll_New]	=	dw_2.Object.mfge_numero[1]
		dw_pdf.Object.espe_codigo[ll_New]	=	dw_2.Object.espe_codigo[1]
		dw_pdf.Object.prod_codigo[ll_New]	=	ll_Productor
		dw_pdf.Object.mfge_fecmov[ll_New]	=	dw_2.Object.mfge_fecmov[1]
		dw_pdf.Object.rehu_secuen[ll_New]	=	ll_Secuencia
		dw_pdf.Object.rehu_archiv[ll_New]	=	ls_Archivo
		ll_Secuencia++
	Next	
Else
	lb_Retorno = False
End If

Return lb_Retorno
end function

public function boolean wf_creaarchivo (string as_archivo);Boolean	lb_Retorno = True
Date		ld_FechaRecepcion
Long		ll_Resultado

ld_FechaRecepcion		=	dw_2.object.mfge_fecmov[1]
ll_Resultado 			= 	dw_docto.Retrieve(Integer(istr_mant.Argumento[1]), Integer(istr_mant.Argumento[2]), &
														Integer(istr_mant.Argumento[3]), Integer(istr_mant.Argumento[10]), &
														ld_FechaRecepcion, ld_FechaRecepcion)

If ll_Resultado > 0 Then
	If dw_docto.SaveAs(as_Archivo, PDF!, False) = -1 Then
		MessageBox('Atención', 'No se pudo generar Documento.')
		lb_Retorno = False
	End If
Else
	MessageBox('Atención', 'No se existe información para generar Documento.')
	lb_Retorno = False
End If

Return lb_Retorno 
end function

public function boolean wf_generadoctopdf ();Boolean	lb_Retorno=	True
Long		ll_Fila

dw_pdf.Reset()
sqlca.AutoCommit	=	False

If wf_GrabaRegistro() Then
	If dw_pdf.Update(True,False) = 1 THEN
			Commit;
									
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				lb_Retorno	=	False
				RollBack;
			ELSE
				dw_pdf.ResetUpdate()
				For ll_Fila = 1 To dw_pdf.RowCount()
					wf_CreaArchivo('c:\'+dw_pdf.Object.rehu_archiv[ll_Fila])
					iuo_HuertoPdf.GrabaImagen(dw_pdf, ll_Fila, Sqlca, 'c:\'+dw_pdf.Object.rehu_archiv[ll_Fila])
					FileDelete('c:\'+dw_pdf.Object.rehu_archiv[ll_Fila])
				Next
			End If
		End If
End If

sqlca.AutoCommit	=	True
Return lb_Retorno
end function

public function boolean wf_validalotectlcal ();Long		ll_Fila, ll_Planilla, ll_Planta, ll_Especie, ll_Numero
Boolean	lb_Retorno = True
String		ls_Mensaje

If dw_3.RowCount() = 0 Then 
	lb_Retorno = False
Else
	For ll_Fila = 1 to dw_3.RowCount()
		ll_Planta	=	dw_3.Object.lote_pltcod[ll_Fila]
		ll_Especie	=	dw_3.Object.lote_espcod[ll_Fila]
		ll_Numero	=	dw_3.Object.lote_codigo[ll_Fila]
		
		SetNull(ll_Planilla)
		
		Select ccre_numero
		  Into :ll_Planilla
		  from dbo.ctlcalrecepcionfrutasenca
		 where plde_codigo = :ll_Planta
		   And espe_codigo = :ll_Especie
		   And lote_codigo = :ll_Numero
		 Using sqlca;
		  
		If sqlca.SQLCode = -1 Then
			F_ErrorBaseDatos(sqlca, "Lectura de Tabla Recepciopn Huerto Control de Calidad")
			lb_Retorno = False
			
		ElseIf sqlca.SQLCode = 100 Then
			lb_Retorno = True
			
		Else
			ls_Mensaje += 'Lote Recepcionado Nro. ' + String(ll_Numero) + ' se encuentra en la planilla de Control de Calidad Nro. : ' + String(ll_Planilla) + '~n'
			lb_Retorno = False
			
		End If	
	Next
	If Not lb_Retorno  Then MessageBox('Atencion.', ls_Mensaje + '~nNo se puede borrar el movimiento.', Exclamation!)
End If

Return lb_Retorno
end function

public subroutine cargamovenv ();Integer	li_rectipenv, li_filadw_5

IF tab_1.tp_1.dw_lotes.RowCount() < 1 THEN RETURN

tab_1.tp_2.dw_envrec.RowsMove(1, tab_1.tp_2.dw_envrec.RowCount(), Primary!, tab_1.tp_2.dw_envrec, tab_1.tp_2.dw_envrec.RowCount(), Delete!)

FOR li_rectipenv = LowerBound(is_enva_tipoen) TO UpperBound(is_enva_tipoen)
	
	IF Integer(is_enva_tipoen[li_rectipenv]) > 0 THEN
		
		li_filadw_5 														= 	tab_1.tp_2.dw_envrec.InsertRow(0)
		tab_1.tp_2.dw_envrec.Object.prod_codigo[li_filadw_5]	=	tab_1.tp_1.dw_lotes.Object.prod_codigo[tab_1.tp_1.dw_lotes.GetRow()]
		tab_1.tp_2.dw_envrec.Object.prod_nombre[li_filadw_5]	=	tab_1.tp_1.dw_lotes.Object.prod_nombre[tab_1.tp_1.dw_lotes.GetRow()]
		tab_1.tp_2.dw_envrec.Object.plde_codigo[li_filadw_5]	=	dw_2.Object.plde_codigo[1]
		tab_1.tp_2.dw_envrec.Object.enva_tipoen[li_filadw_5]	=	Integer(is_enva_tipoen[li_rectipenv])
		tab_1.tp_2.dw_envrec.Object.enva_codigo[li_filadw_5]	=	Integer(is_enva_codigo[li_rectipenv])
		tab_1.tp_2.dw_envrec.Object.cale_calida[li_filadw_5]	=	is_cale_calida[li_rectipenv]
		tab_1.tp_2.dw_envrec.Object.fgme_cantid[li_filadw_5]	=	Integer(is_cantidad[li_rectipenv])
		tab_1.tp_2.dw_envrec.Object.fgme_pesone[li_filadw_5]	=	Dec(is_pesone[li_rectipenv])
		tab_1.tp_2.dw_envrec.Object.fgme_conenv[li_filadw_5]	=	1
		tab_1.tp_2.dw_envrec.Object.clie_codigo[li_filadw_5]	=	dw_2.Object.clie_codigo[1]
		tab_1.tp_2.dw_envrec.Object.cale_nombre[li_filadw_5]	=	is_cale_nombre[li_rectipenv]
		tab_1.tp_2.dw_envrec.Object.fgme_sentid[li_filadw_5]	=	1
		
	END IF
	
NEXT
end subroutine

public subroutine cargaenvases ();Long 			ll_Fila, ll_rectbp, ll_tibapa[], ll_cant_tibapa[], ll_tibapaant, ll_totalbultos
Decimal 		ld_pesos = 0
Integer		li_filas, li_desde, li_hasta, li_UseFind, li_recenv
String 		ls_enva_tipoen[], ls_enva_codigo[], ls_cale_calida[], ls_cantidad[], &
				ls_pesone[], ls_cale_nombre[], ls_prod_codigo[], ls_prod_nombre[]
Boolean		lb_flag, lb_flgtpb
DataWindow	dw_9Mirror, dw_binsmirror

dw_9.SetFilter('')
dw_9.Filter()

dw_9Mirror		=	Create DataWindow
dw_binsmirror	=	Create DataWindow

dw_9Mirror		=	dw_9
dw_binsmirror	=	dw_spro_bins

dw_9Mirror.SetSort("mfgp_tibapa asc, bins_numero asc")
dw_binsmirror.SetSort("mfgp_tibapa asc, bins_numero asc")
dw_9Mirror.Sort()
dw_binsmirror.Sort()

FOR ll_Fila  =  1 TO dw_binsmirror.RowCount()
	lb_flgtpb =	False

	FOR ll_rectbp = 1 TO UpperBound(ll_tibapa[])
		IF ll_tibapa[ll_rectbp] = dw_binsmirror.Object.fgmb_tibapa[ll_fila] THEN
			lb_flgtpb						=	True
			ll_cant_tibapa[ll_rectbp] 	++
			EXIT
		END IF
	NEXT

	IF NOT lb_flgtpb THEN
		ll_tibapa[ll_rectbp+1]			=	dw_binsmirror.Object.fgmb_tibapa[ll_fila]
		ll_cant_tibapa[ll_rectbp+1]	=	1
	END IF 
NEXT

IF dw_9Mirror.RowCount() > 0 THEN
	FOR ll_Fila = 1 TO dw_9Mirror.RowCount()
		
		//INICIO Control de envases
		iuo_bins.Existe(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1], dw_9Mirror.Object.bins_numero[ll_Fila], sqlca, TRUE)

		IF iuo_bins.cale_pesoen > 0 THEN
			lb_flag	=	False
			IF UpperBound(ls_enva_tipoen[]) = 0 THEN
				ls_enva_tipoen[1]		=	String(iuo_bins.enva_tipoen)
				ls_enva_codigo[1]		=	String(iuo_bins.enva_codigo)
				ls_cale_calida[1]		=	iuo_bins.cale_calida
				ls_cantidad[1]			=	"1"
				ls_pesone[1]			=	String(iuo_bins.cale_pesoen)
				ls_cale_nombre[1]		=	iuo_bins.cale_nombre
	
			ELSE
				FOR li_recenv = LowerBound(ls_enva_tipoen[]) TO UpperBound(ls_enva_tipoen[])
					IF ls_enva_tipoen[li_recenv] = String(iuo_bins.enva_tipoen) THEN
						IF ls_enva_codigo[li_recenv] = String(iuo_bins.enva_codigo) THEN
							IF ls_cale_calida[li_recenv] = String(iuo_bins.cale_calida) THEN
	
								ls_cantidad[li_recenv]	=	String(Integer(ls_cantidad[li_recenv]) + 1)
								lb_flag						=	True
								EXIT
	
							END IF
						END IF
					END IF
				NEXT
				
				IF NOT lb_flag THEN
					li_recenv								=	li_recenv + 1
					ls_enva_tipoen[li_recenv]			=	String(iuo_bins.enva_tipoen)
					ls_enva_codigo[li_recenv]			=	String(iuo_bins.enva_codigo)
					ls_cale_calida[li_recenv]			=	iuo_bins.cale_calida
					ls_cantidad[li_recenv]				=	"1"
					ls_pesone[li_recenv]					=	String(iuo_bins.cale_pesoen)
					ls_cale_nombre[li_recenv]			=	iuo_bins.cale_nombre	
				END IF
				
			END IF
		END IF
		//FIN Control de envases
	NEXT
END IF

FOR ll_fila = 1 TO UpperBound(ls_cantidad[])
	ll_totalbultos	=	ll_totalbultos + Long(ls_cantidad[ll_fila])
NEXT

IF ll_totalbultos > 0 THEN
	dw_2.Object.mfge_totbul[1]	=	ll_totalbultos
END IF

IF UpperBound(ll_tibapa[]) > 0 THEN
	FOR ll_Fila = 1 TO UpperBound(ll_tibapa[])
		IF ll_tibapa[ll_Fila] > 0 THEN
			//INICIO Control de base pallets
			iuo_bins.Existe(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1], &
								 ll_tibapa[ll_Fila], sqlca, TRUE)

			lb_flag	=	False
			IF UpperBound(ls_enva_tipoen[]) = 0 THEN
				ls_enva_tipoen[1]		=	String(iuo_bins.enva_tipoen)
				ls_enva_codigo[1]		=	String(iuo_bins.enva_codigo)
				ls_cale_calida[1]		=	iuo_bins.cale_calida
				ls_cantidad[1]			=	String(ll_cant_tibapa[ll_fila])
				ls_pesone[1]			=	String(iuo_bins.cale_pesoen)
				ls_cale_nombre[1]		=	iuo_bins.cale_nombre

			ELSE
				
				FOR li_recenv = LowerBound(ls_enva_tipoen[]) TO UpperBound(ls_enva_tipoen[])
					IF ls_enva_tipoen[li_recenv] = String(iuo_bins.enva_tipoen) THEN
						IF ls_enva_codigo[li_recenv] = String(iuo_bins.enva_codigo) THEN
							IF ls_cale_calida[li_recenv] = String(iuo_bins.cale_calida) THEN
								ls_cantidad[li_recenv]	=	String(Integer(ls_cantidad[li_recenv]) + ll_cant_tibapa[ll_fila])
								lb_flag						=	True
								EXIT
								
							END IF
						END IF
					END IF
				NEXT

				IF NOT lb_flag THEN
					li_recenv							=	li_recenv + 1
					ls_enva_tipoen[li_recenv]		=	String(iuo_bins.enva_tipoen)
					ls_enva_codigo[li_recenv]		=	String(iuo_bins.enva_codigo)
					ls_cale_calida[li_recenv]		=	iuo_bins.cale_calida
					ls_cantidad[li_recenv]			=	String(ll_cant_tibapa[ll_fila])
					ls_pesone[li_recenv]				=	String(iuo_bins.cale_pesoen)
					ls_cale_nombre[li_recenv]		=	iuo_bins.cale_nombre
				END IF

			END IF
			
			//FIN Control de envases
		END IF
	NEXT
END IF

//Asignación de envases recepcionados
is_enva_tipoen[]	=	ls_enva_tipoen[]
is_enva_codigo[]	=	ls_enva_codigo[]
is_cale_calida[]	=	ls_cale_calida[]
is_cantidad[]		=	ls_cantidad[]
is_pesone[]			=	ls_pesone[]
is_cale_nombre[]	=	ls_cale_nombre[]

CargaMovEnv()
end subroutine

public function boolean eliminacion_packing ();Integer	li_cliente, li_tipo, li_cuenta
Long		ll_planta, ll_proceso

IF gstr_paramplanta.packing THEN
	
	SQLCA.AutoCommit	=	False
	
	li_cliente 			= 	dw_2.GetItemNumber(1, "clie_codigo", Delete!, False)
	ll_planta 			= 	dw_6.GetItemNumber(1, "lote_pltcod", Delete!, False)
	ll_proceso			= 	dw_6.GetItemNumber(1, "lote_codigo", Delete!, False)
	li_tipo				= 	4
	
	Select Count(*)
	  Into :li_cuenta
	  from dbo.spro_palletfruta
	 where clie_codigo = :li_cliente and
			 plde_codigo = :ll_planta 	and
			 pafr_tipdoc = :li_tipo	 	and
			 pafr_docrel = :ll_proceso;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_cajasprod")
		Return FALSE
		
	ELSEIF li_cuenta > 0 THEN
		MessageBox("Atención","Imposible eliminar la recepción solicitada, "+&
									 "ya que esta asociada a "+String(li_cuenta)+" cajas.", &
									 Exclamation!)
		Return FALSE
	END IF
	
	Delete dbo.spro_cajasprod
	 where clie_codigo = :li_cliente and
			 plde_codigo = :ll_planta 	and
			 capr_tipdoc = :li_tipo	 	and
			 capr_docrel = :ll_proceso;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_cajasprod")
		Return FALSE
	END IF
			 
	Delete dbo.spro_resulpackingdeta
	 Where clie_codigo = :li_cliente and
			 plde_codigo = :ll_planta 	and
			 orpr_tipdoc = :li_tipo	 	and
			 orpr_numero = :ll_proceso;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_resulpackingdeta")
		Return FALSE
	END IF
	
	Delete dbo.spro_resulfrutemba
	 Where clie_codigo = :li_cliente and
			 plde_codigo = :ll_planta 	and
			 orpr_tipdoc = :li_tipo	 	and
			 orpr_numero = :ll_proceso;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_resulfrutemba")
		Return FALSE
	END IF
	
	Delete dbo.spro_resulfrutcomer
	 Where clie_codigo = :li_cliente and
			 plde_codigo = :ll_planta 	and
			 orpr_tipdoc = :li_tipo	 	and
			 orpr_numero = :ll_proceso;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_resulfrutcomer")
		Return FALSE
	END IF
	
	Delete dbo.spro_resultpacking
	 Where clie_codigo = :li_cliente and
			 plde_codigo = :ll_planta 	and
			 orpr_tipdoc = :li_tipo	 	and
			 orpr_numero = :ll_proceso;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_resultpacking")
		Return FALSE
	END IF
	
	Delete dbo.spro_ordenprocvacdeta
	 Where clie_codigo = :li_cliente and
			 plde_codigo = :ll_planta 	and
			 orpr_tipord = :li_tipo	 	and
			 orpr_numero = :ll_proceso;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenprocvacdeta")
		Return FALSE
	END IF
	
	Delete dbo.spro_ordenprocvacenca
	 Where clie_codigo = :li_cliente and
			 plde_codigo = :ll_planta 	and
			 orpr_tipord = :li_tipo	 	and
			 orpr_numero = :ll_proceso;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenprocvacenca")
		Return FALSE
	END IF
	
	Delete dbo.spro_movtofrutagrandeta
	  From dbo.spro_movtofrutagrandeta as mgd, dbo.spro_movtofrutagranenca as mge
	 Where mge.clie_codigo = mgd.clie_codigo 	and
			 mge.plde_codigo = mgd.plde_codigo	and
			 mge.tpmv_codigo = mgd.tpmv_codigo	and
			 mge.mfge_numero = mgd.mfge_numero 	and
			 mge.clie_codigo = :li_cliente 		and
			 mge.plde_codigo = :ll_planta 		and
			 mge.defg_tipdoc = :li_tipo	 		and
			 mge.defg_docrel = :ll_proceso;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutagrandeta")
		Return FALSE
	END IF
	
	Delete dbo.spro_movtofrutagranenca
	 Where clie_codigo = :li_cliente 		and
			 plde_codigo = :ll_planta 		and
			 defg_tipdoc = :li_tipo	 		and
			 defg_docrel = :ll_proceso;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutagranenca")
		Return FALSE
	END IF
	
	Delete dbo.spro_ordenprocdeta
	 Where clie_codigo = :li_cliente and
			 plde_codigo = :ll_planta 	and
			 orpr_tipord = :li_tipo	 	and
			 orpr_numero = :ll_proceso;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenprocdeta")
		Return FALSE
	END IF
	
	Delete dbo.spro_ordenproceso
	 Where clie_codigo = :li_cliente and
			 plde_codigo = :ll_planta 	and
			 orpr_tipord = :li_tipo	 	and
			 orpr_numero = :ll_proceso;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenproceso")
		Return FALSE
	END IF
	
	Delete dbo.spro_terminoproceso
	 Where clie_codigo = :li_cliente and
			 plde_codigo = :ll_planta 	and
			 tpmv_codigo = 21			 	and
			 lote_codigo = :ll_proceso;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenproceso")
		Return FALSE
	END IF
	
END IF

Return True
end function

public function boolean datos_correo ();Integer li_codigo

SELECT 	empr_codplt
INTO		:li_codigo
FROM 		dbo.parempresa;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla parempresa")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	Return True
END IF

SELECT 	plde_correo
INTO		:is_correo
FROM 		dbo.plantadesp
WHERE 	plde_codigo = :li_codigo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla plantadesp")
	Return True
ELSEIF is_correo = '' OR Isnull(is_correo) THEN
	MessageBox("Atención","Falta Correo en Tabla Planta Código "+String(li_codigo))
	is_correo = '<pvaldes@rioblanco.cl>;<spison@rioblanco.cl>'
	Return True
END IF

Return False
end function

public function any tipo_update_lote (long al_planta, integer ai_especie, long al_lote, integer ai_tipenv, integer ai_codenv);Integer	li_existe

SELECT Count(*)
  INTO :li_existe
  FROM dbo.spro_lotesfrutagrandeta
 WHERE lote_pltcod = :al_planta
   AND lote_espcod = :ai_especie
	AND lote_codigo = :al_lote
	AND enva_tipoen = :ai_tipenv
	AND enva_codigo = :ai_codenv
 USING SQLCA;
 
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_lotesfrutagrandeta")
	Return New!
	
ELSEIF li_existe < 1 OR IsNull(li_existe) THEN
	Return NewModified!
	
ELSE
	Return DataModified!

END IF
end function

on w_maed_movtofrutagranel_mantrecepcion.create
int iCurrent
call super::create
this.dw_4=create dw_4
this.cb_guia=create cb_guia
this.dw_9=create dw_9
this.dw_exidetaborra=create dw_exidetaborra
this.dw_exismovtodetanulos=create dw_exismovtodetanulos
this.dw_exideta=create dw_exideta
this.dw_exiencab=create dw_exiencab
this.dw_envases_comer=create dw_envases_comer
this.dw_lotescategoria=create dw_lotescategoria
this.dw_pdf=create dw_pdf
this.dw_docto=create dw_docto
this.dw_desverd=create dw_desverd
this.dw_spro_bins=create dw_spro_bins
this.ole_puerta=create ole_puerta
this.tab_1=create tab_1
this.dw_6=create dw_6
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_4
this.Control[iCurrent+2]=this.cb_guia
this.Control[iCurrent+3]=this.dw_9
this.Control[iCurrent+4]=this.dw_exidetaborra
this.Control[iCurrent+5]=this.dw_exismovtodetanulos
this.Control[iCurrent+6]=this.dw_exideta
this.Control[iCurrent+7]=this.dw_exiencab
this.Control[iCurrent+8]=this.dw_envases_comer
this.Control[iCurrent+9]=this.dw_lotescategoria
this.Control[iCurrent+10]=this.dw_pdf
this.Control[iCurrent+11]=this.dw_docto
this.Control[iCurrent+12]=this.dw_desverd
this.Control[iCurrent+13]=this.dw_spro_bins
this.Control[iCurrent+14]=this.ole_puerta
this.Control[iCurrent+15]=this.tab_1
this.Control[iCurrent+16]=this.dw_6
end on

on w_maed_movtofrutagranel_mantrecepcion.destroy
call super::destroy
destroy(this.dw_4)
destroy(this.cb_guia)
destroy(this.dw_9)
destroy(this.dw_exidetaborra)
destroy(this.dw_exismovtodetanulos)
destroy(this.dw_exideta)
destroy(this.dw_exiencab)
destroy(this.dw_envases_comer)
destroy(this.dw_lotescategoria)
destroy(this.dw_pdf)
destroy(this.dw_docto)
destroy(this.dw_desverd)
destroy(this.dw_spro_bins)
destroy(this.ole_puerta)
destroy(this.tab_1)
destroy(this.dw_6)
end on

event open;/* Argumentos
istr_mant.argumento[1] = Código Planta
istr_mant.argumento[2] = Tipo de Movimiento
istr_mant.argumento[3] = Número de Despacho
istr_mant.argumento[4] = Sentido del Movimiento de Envases
istr_mant.argumento[5] = Especie
istr_mant.argumento[6] = Productor no se asigna / va en el detalle del lote
istr_mant.argumento[7] = Ultimo Lote Ocupado
istr_mant.argumento[8] = decimales especie ( 0 - 2 ) 
istr_mant.argumento[9] = Parametro para definir si es [M]antencion o [R]ecepcion
istr_mant.argumento[10] = Código Cliente
*/
Integer	li_resultado
String	ls_parametros, ls_nombre

x				= 0
y				= 0
This.Height	= 2500
im_menu		= m_principal

This.Icon										=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_2.Object.oproceso.visible 		= 	gb_RecepcionDeProceso
dw_2.Object.defg_docrel.visible 	= 	gb_RecepcionDeProceso
dw_2.Object.ordenproceso.visible 	= 	gb_RecepcionDeProceso



If NOT gb_RecepcionDeProceso Then
	This.Title = "RECEPCION DE HUERTO"
	ii_TipMov 								= 	1
Else
	This.Title = "RECEPCION DE PREPROCESO"
	ii_TipMov 								= 	8
End If

If gstr_ParamPlanta.etiquetaembalaje = 0 Then
	Tab_1.Tp_1.dw_lotes.DataObject	=	"dw_mues_spro_lotesfrutagranel_rec_kguia"
End If

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

istr_mant.argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_mant.argumento[2]	=	String(ii_TipMov)
istr_mant.argumento[4]	=	'1'
istr_mant.argumento[5]	=	String(gstr_ParamPlanta.CodigoEspecie)
istr_mant.argumento[7]	=	'0'
istr_mant.argumento[9] 	=	'M'
istr_mant.argumento[10] =	String(gi_codexport)

dw_spro_bins.SetTransObject(sqlca)

dw_2.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve() 

dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
If idwc_especie.Retrieve() = 0 Then
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
Else
	idwc_especie.SetSort("espe_nombre A")
	idwc_especie.Sort()
End If

dw_2.GetChild("tran_codigo", idwc_Transp)
idwc_Transp.SetTransObject(sqlca)

If idwc_Transp.Retrieve() = 0 Then
	MessageBox("Atención", "Falta Registrar Transportistas")
	idwc_Transp.InsertRow(0)
Else
	idwc_Transp.SetSort("tran_nombre A")
	idwc_Transp.Sort()	
End If

dw_1.GetChild("cama_codigo",idwc_Camara)
idwc_Camara.SetTransObject(SQLCA)
idwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta)

dw_1.GetChild("lote_espcod", idwc_especiedet2)
idwc_especiedet2.SetTransObject(sqlca)
If idwc_especiedet2.Retrieve() = 0 Then
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especiedet2.InsertRow(0)
Else
	idwc_especiedet2.SetSort("espe_nombre A")
	idwc_especiedet2.Sort()
End If

dw_3	=	Tab_1.Tp_1.dw_lotes
dw_5	=	Tab_1.Tp_2.dw_envrec
dw_7	=	Tab_1.Tp_3.dw_envdes

dw_3.GetChild("prbr_codpre",idwc_Predio)
idwc_Predio.SetTransObject(SQLCA)
idwc_Predio.InsertRow(0)

dw_3.GetChild("vari_codigo",idwc_Variedad)
idwc_Variedad.SetTransObject(SQLCA)
idwc_Variedad.Retrieve(gstr_ParamPlanta.CodigoEspecie)

dw_4.GetChild("plde_codigo",idwc_plantadw4)
idwc_plantadw4.SetTransObject(SQLCA)
idwc_plantadw4.Retrieve()

dw_4.GetChild("plde_coorde",idwc_plancodw4)
idwc_plancodw4.SetTransObject(SQLCA)
idwc_plancodw4.Retrieve()

dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_7.SetTransObject(sqlca)
dw_9.SetTransObject(sqlca)
dw_pdf.SetTransObject(sqlca)
dw_docto.SetTransObject(sqlca)
dw_desverd.SetTransObject(sqlca)

dw_1.GetChild("lote_pltcod",idwc_plantadw1)
idwc_plantadw1.SetTransObject(SQLCA)
idwc_plantadw1.Retrieve()

dw_1.ModIfy("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.ModIfy("DataWindow.Footer.Height = 84")

dw_3.ModIfy("datawindow.message.title='Error '+ is_titulo")
dw_3.ModIfy("DataWindow.Footer.Height = 84")

dw_5.ModIfy("datawindow.message.title='Error '+ is_titulo")
dw_5.ModIfy("DataWindow.Footer.Height = 84")

dw_7.ModIfy("datawindow.message.title='Error '+ is_titulo")
dw_7.ModIfy("DataWindow.Footer.Height = 84")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.solo_consulta			=	False

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
							
iuo_Transport			=	Create uo_transportista
iuo_Camion				=	Create uo_camiones
iuo_Especie				=	Create uo_especie
iuo_Correlativo			=	Create uo_LotesCorrel
iuo_PesoEstanEspe	=	Create uo_PesoEstanEspe
iuo_FechaMovto		=	Create uo_FechaMovto
iuo_tipomovtofruta		=	Create uo_tipomovtofruta
iuo_TipoMovtoEnva	=	Create uo_tipomovtofruta
iuo_calicosechero		=  Create uo_calicosechero 
iuo_bins					=	Create uo_bins
iuo_HuertoPDF			=	Create uo_RecepcionhuertoPDF

If NOT IsNull(gstr_paramplanta.Password) AND Trim(gstr_paramplanta.Password) <> '' Then
	PostEvent("ue_validapassword")
End If

end event

event resize;call super::resize;Tab_1.x								=	dw_1.x
Tab_1.y								=	dw_1.y
Tab_1.Width						=	dw_1.Width
Tab_1.Height						=	dw_1.Height

Tab_1.Tp_1.dw_lotes.x			= 27
Tab_1.Tp_1.dw_lotes.y			= 36
Tab_1.Tp_1.dw_lotes.height	= Tab_1.height - 180
Tab_1.Tp_1.dw_lotes.width		= Tab_1.width - 92

Tab_1.Tp_2.dw_envrec.x		= 27
Tab_1.Tp_2.dw_envrec.y		= 36
Tab_1.Tp_2.dw_envrec.height	= Tab_1.height - 180
Tab_1.Tp_2.dw_envrec.width	= Tab_1.width - 92

Tab_1.Tp_3.dw_envdes.x		= 27
Tab_1.Tp_3.dw_envdes.y		= 36
Tab_1.Tp_3.dw_envdes.height	= Tab_1.height - 180
Tab_1.Tp_3.dw_envdes.width	= Tab_1.width - 92

cb_guia.x 							=  pb_salir.x
cb_guia.y 							=  pb_salir.y + pb_salir.Height + 10
end event

event ue_borra_detalle;call super::ue_borra_detalle;Integer	li_tabpage, li_borra
Boolean	lb_estado
str_mant_envases	lstr_mant

li_tabpage	=	tab_1.SelectedTab

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

lb_estado			=	istr_mant.Solo_Consulta

IF li_tabpage <> 1 THEN
	lstr_mant.argumento				=	istr_mant.argumento
	lstr_mant.dw						=	istr_mant.dw
	lstr_mant.dw2						=	istr_mant.dw2
	lstr_mant.Agrega					=	istr_mant.Agrega
	lstr_mant.Borra					=	istr_mant.Borra
	lstr_mant.Solo_Consulta			=	istr_mant.Solo_Consulta
	lstr_mant.respuesta				=	istr_mant.respuesta
	lstr_mant.tipo						=	istr_mant.tipo
	lstr_mant.usuariosoloconsulta	=	istr_mant.usuariosoloconsulta
	
	ProductoresLotes(lstr_mant.productores)
END IF

CHOOSE CASE li_tabpage
	CASE 1 
		IF NOT verificalote(dw_3.Object.lote_pltcod[il_fila],&
									dw_3.Object.lote_espcod[il_fila],&
									dw_3.Object.lote_codigo[il_fila]) THEN
			Message.DoubleParm = -1
			RETURN
		END IF	
		istr_mant.dw	=	dw_3
		istr_mant.dw2	=	dw_6
		OpenWithParm(iw_mantencion_1, istr_mant)
	CASE 2
		lstr_mant.dw	=	dw_5
		SetNull(lstr_mant.dw2)
		lstr_mant.Argumento[4]	=	'1'
		OpenWithParm(iw_mantencion_2, lstr_mant)
	CASE 2
		lstr_mant.dw	=	dw_7
		SetNull(lstr_mant.dw2)
		lstr_mant.Argumento[4]	=	'2'
		OpenWithParm(iw_mantencion_2, lstr_mant)
		
END CHOOSE

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	CHOOSE CASE li_tabpage
		CASE 1
			li_borra	=	dw_3.DeleteRow(0)
			IF li_Borra = 1 THEN
				li_Borra = dw_6.RowsMove(1,dw_6.RowCount(),Primary!,dw_6,1,Delete!)
			END IF
		CASE 2
			li_borra	=	dw_5.DeleteRow(0)
		CASE 3
			li_borra	=	dw_7.DeleteRow(0)
	END CHOOSE
 
	IF li_borra = 1 THEN
		dw_6.SetFilter("")
		dw_6.Filter()
		
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_3.RowCount() = 0 THEN pb_eli_det.Enabled = False
END IF

istr_mant.borra	 = False

captura_totales()
ProductoresLotes(lstr_mant.productores)
end event

event ue_borrar;Integer	li_Cliente,li_aplicaenvase
If dw_2.RowCount() < 1 Then RETURN

If Not wf_ValidaLoteCtlCal() Then Return

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

If Message.DoubleParm = -1 Then RETURN

li_Cliente	=	dw_2.Object.clie_codigo[1]

SELECT 	clie_conexi, cone_codigo
INTO   	:il_conexiste, :il_coneccion
FROM dbo.clientesprod
WHERE clie_codigo = :li_Cliente;

//If il_conexiste = 1 Then
//	sqlexi	=	CREATE Transaction
//	
//	If Conexionexistencia() Then
//		dw_exideta.SetTransObject(sqlexi)
//		dw_exiencab.SetTransObject(sqlexi)	
//		dw_exismovtodetanulos.SetTransObject(sqlexi)
//		dw_exidetaborra.SetTransObject(sqlexi)
//		TriggerEvent("anulamovto")
//		DISCONNECT USING sqlexi;
//		
//		SELECT isnull(plde_renenv,0)
//			INTO :li_aplicaenvase
//			FROM dbo.plantadesp 
//			WHERE plde_codigo = :il_packing;
//		
//		If li_aplicaenvase = 1 Then
//			If Conexionexistencia() Then
//				dw_exideta.SetTransObject(sqlexi)
//				dw_exiencab.SetTransObject(sqlexi)	
//				dw_exismovtodetanulos.SetTransObject(sqlexi)
//				dw_exidetaborra.SetTransObject(sqlexi)
//				TriggerEvent("anulamovto2")
//				
//				DISCONNECT USING sqlexi;
//				
//			Else
//				MessageBox("Atención", "Imposible Conectar con Base de Existencia(Generar Despacho).",Exclamation!, OK!)
//			End If
//		End If	
//	Else
//		MessageBox("Atención", "Imposible Conectar con Base de Existencia.",Exclamation!, OK!)
//	End If
//End If	

If dw_1.RowCount() > 0 Then dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)
If dw_3.RowCount() > 0 Then dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)
If dw_4.RowCount() > 0 Then dw_4.RowsMove(1,dw_4.RowCount(),Primary!,dw_4,1,Delete!)
If dw_5.RowCount() > 0 Then dw_5.RowsMove(1,dw_5.RowCount(),Primary!,dw_5,1,Delete!)
If dw_6.RowCount() > 0 Then dw_6.RowsMove(1,dw_6.RowCount(),Primary!,dw_6,1,Delete!)
If dw_7.RowCount() > 0 Then dw_7.RowsMove(1,dw_7.RowCount(),Primary!,dw_7,1,Delete!)
If dw_9.RowCount() > 0 Then dw_9.RowsMove(1,dw_9.RowCount(),Primary!,dw_9,1,Delete!)
If dw_pdf.RowCount() > 0 Then dw_pdf.RowsMove(1,dw_pdf.RowCount(),Primary!,dw_pdf,1,Delete!)
If dw_spro_bins.RowCount() > 0 Then dw_spro_bins.RowsMove(1,dw_spro_bins.RowCount(),Primary!,dw_spro_bins,1,Delete!)

If dw_2.DeleteRow(0) = 1 Then
	ib_borrar = False
	w_main.SetMicroHelp("Borrando Registro...")
	
	ib_AutoCommit		=	SQLCA.AutoCommit
	SQLCA.AutoCommit	=	False
	
	If wf_actualiza_db(True) Then
		w_main.SetMicroHelp("Registro Borrado...")
		This.TriggerEvent("ue_nuevo")
		SetPointer(Arrow!)
	Else
		w_main.SetMicroHelp("Registro no Borrado...")
	End If			
Else
	ib_borrar = False
	MessageBox(This.Title,"No se puede borrar actual registro.")
End If
end event

event ue_guardar;Integer li_cliente, li_aplicaenvase
String	ls_numero
Date		ld_fecha

IF dw_3.AcceptText() = -1 THEN RETURN
IF dw_4.AcceptText() = -1 THEN RETURN
IF dw_5.AcceptText() = -1 THEN RETURN
IF dw_1.AcceptText() = -1 THEN RETURN

ls_numero	= String(dw_2.Object.mfge_numero[1])
ld_fecha 		= dw_2.Object.mfge_fecmov[1]

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
	
	//Crea conexion hacia existencia
	//Crea conexion hacia existencia
	
	IF NOT gb_RecepcionDeProceso THEN
		IF dw_2.Object.mfge_estmov[1] = 3 THEN
			li_Cliente	=	dw_2.Object.clie_codigo[1]
			
//			SELECT 	clie_conexi, cone_codigo
//			INTO   	:il_conexiste, :il_coneccion
//			FROM dbo.clientesprod
//			WHERE clie_codigo = :li_Cliente;
				
//			IF il_conexiste = 1 THEN
//				sqlexi	=	CREATE Transaction
//				iuo_grabatablabitacora			=	Create uo_grabatablabitacora
//				Datos_correo()
//				
//				IF Conexionexistencia() THEN
//					dw_exideta.SetTransObject(sqlexi)
//					dw_exiencab.SetTransObject(sqlexi)	
//					dw_exismovtodetanulos.SetTransObject(sqlexi)
//					dw_exidetaborra.SetTransObject(sqlexi)
//					TriggerEvent("ue_despuesborrar")
//					TriggerEvent("ue_despuesguardar")
//					
//					IF li_retorno = 2 THEN
//						iuo_grabatablabitacora.genera_bitacora(ls_numero,ld_fecha,This.Title,'','','',&
//						'Verificar Datos en Base de Existencia '+is_base,is_base,1,is_error,1,il_coderror,&
//						is_correo,'Problema Control de Envases Recepción Granel '+is_base,'Movimiento N° '+ls_numero+' Mantenedor '+This.Title+' con problemas, usuario '+gstr_us.Nombre+' Computador '+gstr_us.computador +', Error '+is_error)
//						is_error = ''	
//					END IF		
//					
//					DISCONNECT USING sqlexi;
//					
//					IF isnull(il_packing) OR il_packing = 0 THEN
//						iuo_grabatablabitacora.genera_bitacora(ls_numero,ld_fecha,This.Title,'','','',&
//						'Verificar Packing del Productor '+is_base,is_base,1,'Falta Packing Productor',1,0,&
//						is_correo,'Problema Control de Envases Recepción Granel '+is_base,'Movimiento N° '+ls_numero+' Mantenedor '+This.Title+' con problemas, usuario '+gstr_us.Nombre+' Computador '+gstr_us.computador +', Error '+'Falta Packing Productor')
//						is_error = ''	
//						IF Message.DoubleParm = -1 THEN RETURN
//
//						IF il_NumFruta>0 AND dw_2.GetItemStatus(1, 0, Primary!) = NotModified! THEN
//							il_NumFruta = 0
//							il_NumEnva	= 0
//						END IF
//						
//						IF dw_2.Object.mfge_estmov[1]	=	3 THEN
//							wf_GeneraDoctoPDF()
//							ActualizaKilosLote()
//						END IF
//						
//						IF gstr_paramplanta.packing THEN//Entra a proceso automatico
//							ProcesoPacking()
//						END IF
//						Return	
//					END IF	
//					
//					IF li_retorno <> 2 THEN
//						SELECT isnull(plde_renenv,0)
//							INTO :li_aplicaenvase
//							FROM dbo.plantadesp
//							WHERE plde_codigo = :il_packing;
//						
//						IF li_aplicaenvase = 1 THEN
//							IF Conexionexistencia() THEN
//								dw_exideta.SetTransObject(sqlexi)
//								dw_exiencab.SetTransObject(sqlexi)	
//								dw_exismovtodetanulos.SetTransObject(sqlexi)
//								dw_exidetaborra.SetTransObject(sqlexi)
//								TriggerEvent("ue_despuesborrar2")
//								TriggerEvent("ue_despuesguardar2")
//								
//								IF li_retorno = 2 THEN
//									iuo_grabatablabitacora.genera_bitacora(ls_numero,ld_fecha,This.Title,'','','',&
//									'Verificar Datos en Base de Existencia '+is_base,is_base,1,is_error,1,il_coderror,&
//									is_correo,'Problema Control de Envases Recepción Granel '+is_base,'Movimiento N° '+ls_numero+' Mantenedor '+This.Title+' con problemas, usuario '+gstr_us.Nombre+' Computador '+gstr_us.computador +', Error '+is_error)
//									li_retorno = 0	
//								ELSE	
//									iuo_grabatablabitacora.actualizaestado(ls_numero,ld_fecha,This.Title)
//									
//								END IF
//								DISCONNECT USING sqlexi;
//								
//							ELSE
//								iuo_grabatablabitacora.genera_bitacora(ls_numero,ld_fecha,This.Title,'','','',&
//								'Verificar Conexión a Base '+is_base,is_base,1,'No Se Pudo Conectar el Sistema a Base de Datos de Existencia',1,il_coderror,&
//								is_correo,'Problema Control de Envases Recepción Granel '+is_base,'Movimiento N° '+ls_numero+' Con problemas, Error en Conexión con Base de Existencia '+' Mantenedor '+This.Title+' con problemas, usuario '+gstr_us.Nombre+' Computador '+gstr_us.computador +', Error '+is_error)
//								MessageBox("Atención", "Imposible Conectar con Base de Existencia.",Exclamation!, OK!)
//							END IF
//						ELSE
//							MessageBox("Cuenta Corriente Envases", "Packing "+String(il_packing)+" NO Tiene Marca de Control de Envases a Packing Origen, NO Generá Movimiento en Existencia.",Exclamation!, OK!)	
//						END IF
//					END IF	
//				ELSE
//					iuo_grabatablabitacora.genera_bitacora(ls_numero,ld_fecha,This. Title,'','','',&
//					'Verificar Conexión a Base '+is_base,is_base,1,'No Se Pudo Conectar el Sistema a Base de Datos de Existencia',1,il_coderror,&
//					is_correo,'Problema Control de Envases Recepción Granel '+is_base,'Movimiento N° '+ls_numero+' Con problemas, Error en Conexión con Base de Existencia '+' Mantenedor '+This.Title+' con problemas, usuario '+gstr_us.Nombre+' Computador '+gstr_us.computador +', Error '+is_error)
//					MessageBox("Atención", "Imposible Conectar con Base de Existencia.",Exclamation!, OK!)
//				END IF
//			END IF	
		END IF	
	END IF	
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF

IF Message.DoubleParm = -1 THEN RETURN

IF il_NumFruta>0 AND dw_2.GetItemStatus(1, 0, Primary!) = NotModified! THEN
	il_NumFruta = 0
	il_NumEnva	= 0
END IF

IF dw_2.Object.mfge_estmov[1]	=	3 THEN
	wf_GeneraDoctoPDF()
	ActualizaKilosLote()
END IF

IF gstr_paramplanta.packing THEN//Entra a proceso automatico
	ProcesoPacking()
END IF
end event

event ue_modifica_detalle;Integer	li_tabpage
Boolean	lb_estado

str_mant_envases	lstr_mant

li_tabpage	=	Tab_1.SelectedTab

istr_mant.agrega	= False
istr_mant.borra		= False

lb_estado			=	istr_mant.Solo_Consulta

If li_tabpage <> 1 Then
	lstr_mant.argumento				=	istr_mant.argumento
	lstr_mant.dw						=	istr_mant.dw
	lstr_mant.dw2						=	istr_mant.dw2
	lstr_mant.Agrega					=	istr_mant.Agrega
	lstr_mant.Borra						=	istr_mant.Borra
	lstr_mant.Solo_Consulta			=	istr_mant.Solo_Consulta
	lstr_mant.respuesta				=	istr_mant.respuesta
	lstr_mant.tipo						=	istr_mant.tipo
	lstr_mant.usuariosoloconsulta	=	istr_mant.usuariosoloconsulta
	
	ProductoresLotes(lstr_mant.productores)
End If

CHOOSE CASE li_tabpage
	CASE 1
		If dw_3.RowCount() > 0 Then
			istr_mant.dw				=	dw_3
			istr_mant.dw2				=	dw_6
			istr_mant.argumento[7]	=	String(dw_3.Object.lote_codigo[il_fila])
			dw_spro_bins.SetFilter("lote_codigo 	= " + istr_mant.argumento[7])
			dw_spro_bins.Filter()
			
			OpenWithParm(iw_mantencion_1, istr_mant)
			
			dw_spro_bins.SetFilter('')
			dw_spro_bins.Filter()
			
			If NOT gstr_paramplanta.bultobins Then
				If Not gstr_paramplanta.packing  Then AsignaPeso()
			End If
			
			If gstr_paramplanta.palletdebins Then
				AsignaPeso()
				CargaEnvases()
				
			End If
		End If
		istr_mant.Solo_Consulta	=	lb_estado

	CASE 2				
		If dw_5.RowCount() > 0 Then
			lstr_mant.dw	=	dw_5
			SetNull(lstr_mant.dw2)
			lstr_mant.Argumento[4]	=	'1'
			OpenWithParm(iw_mantencion_2, lstr_mant)
		End If
		lstr_mant.Solo_Consulta	=	lb_estado
		
	CASE 3				
		If dw_7.RowCount() > 0 Then
			lstr_mant.dw	=	dw_7
			SetNull(lstr_mant.dw2)
			lstr_mant.Argumento[4]	=	'2'
			OpenWithParm(iw_mantencion_2, lstr_mant)
		End If
End CHOOSE

dw_6.SetFilter("")
dw_6.Filter()

captura_totales()
ProductoresLotes(lstr_mant.productores)
end event

event ue_nuevo;Long		ll_modif
Integer  li_cliente

is_rut		=	''
is_rutprod	=	''

ib_ok	= True

//li_cliente = dw_2.object.clie_codigo[1]
istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta


IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif	=	dw_1.GetNextModified(0, Primary!)
			ll_modif	+=	dw_2.GetNextModified(0, Primary!)
			ll_modif	+=	dw_3.GetNextModified(0, Primary!)
			ll_modif	+=	dw_6.GetNextModified(0, Primary!)
			ll_modif	+=	dw_5.GetNextModified(0, Primary!)
			ll_modif	+=	dw_7.GetNextModified(0, Primary!)
		
			IF dw_1.RowCount() > 0 and ll_modif > 0 THEN
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
dw_3.Reset()
dw_6.Reset()
dw_4.Reset()
dw_5.Reset()
dw_7.Reset()

dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.Object.buscacamion.Visible	=	0
dw_2.Object.camion.Visible 		=	0
dw_2.Object.carro.Visible			=	0
dw_2.Object.romanacamion.Visible =	0
dw_2.Object.romanacarro.Visible	=	0
dw_2.SetRedraw(True)

dw_2.SetFocus()


dw_2.object.clie_codigo[1] 		=  gi_codexport
istr_mant.argumento[10]    		=  String(gi_codexport)
dw_2.Object.plde_codigo[1]		=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.tpmv_codigo[1]	=	ii_TipMov
dw_2.Object.espe_codigo[1]	=	gstr_ParamPlanta.CodigoEspecie

idt_FechaSistema	=	F_FechaHora()

dw_2.Object.mfge_fecmov[1]	=	Date(idt_FechaSistema)
dw_2.Object.refg_horaen[1]	=	Time(idt_FechaSistema)	

dw_2.Object.destare.Visible		=	False
dw_2.Object.destare.Text		=	'Salida'
cb_guia.Enabled					=	False
ib_Salida								=	False
ib_Destare							=	False

istr_Mant.Argumento[3]			=	''
istr_mant.Argumento[5]			=	String(dw_2.Object.espe_codigo[1])
istr_mant.Argumento[7]			=	'0'
		
IF iuo_especie.existe(gstr_ParamPlanta.CodigoEspecie,TRUE, sqlca) THEN
	IF iuo_especie.kildec = 1 THEN
		ii_kildec = 2
	ELSE
		ii_kildec = 0
	END IF	
END IF	
		
tab_1.tp_1.Enabled	=	False
tab_1.tp_2.Enabled	=	False
tab_1.tp_3.Enabled	=	False
pb_eliminar.Enabled  =	False
pb_grabar.Enabled	=	False
pb_ins_det.Enabled	=	False
pb_imprimir.Enabled	=	False
pb_eli_det.Enabled	=	False
cb_guia.Enabled		=	False

tab_1.SelectTab(1)
HabilitaEncab(True)		
end event

event ue_nuevo_detalle;Integer	li_tabpage
Boolean	lb_estado

str_mant_envases	lstr_mant

li_TabPage			=	Tab_1.SelectedTab

istr_mant.Borra	=	False
istr_mant.Agrega	=	True
	
lb_estado			=	istr_mant.Solo_Consulta

IF li_TabPage <> 1 THEN
	lstr_mant.argumento				=	istr_mant.argumento
	lstr_mant.dw						=	istr_mant.dw
	lstr_mant.dw2						=	istr_mant.dw2
	lstr_mant.Agrega					=	istr_mant.Agrega
	lstr_mant.Borra						=	istr_mant.Borra
	lstr_mant.Solo_Consulta			=	istr_mant.Solo_Consulta
	lstr_mant.respuesta				=	istr_mant.respuesta
	lstr_mant.tipo						=	istr_mant.tipo
	lstr_mant.usuariosoloconsulta	=	istr_mant.usuariosoloconsulta
	
	ProductoresLotes(lstr_mant.productores)
END IF

CHOOSE CASE li_tabpage
	CASE 1
		IF Integer(istr_mant.argumento[7]) = 0 THEN
			iuo_Correlativo.Obtiene_Correlativo(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[5]), True, SQLCA)
			istr_mant.argumento[7]			=	String(iuo_Correlativo.Correl_LoteFG)
		END IF
		istr_mant.Argumento[11] = String(dw_2.Object.mfge_totbul[1])
		
		istr_mant.dw	=	dw_3
		istr_mant.dw2	=	dw_6
		OpenWithParm(iw_mantencion_1, istr_mant)
		istr_mant.Solo_Consulta	=	lb_estado
		IF NOT gstr_paramplanta.BultoBins THEN AsignaPeso()
		
		istr_mant	=	Message.PowerObjectParm
		
		dw_3.SetRow(il_Fila)
		dw_3.SelectRow(il_Fila, True)
		
	CASE 2
		lstr_mant.dw	=	dw_5
		SetNull(lstr_mant.dw2)
		lstr_mant.Argumento[4]	=	'1'
		OpenWithParm(iw_mantencion_2, lstr_mant)
		lstr_mant.Solo_Consulta	=	lb_estado
		dw_5.SetRow(il_Fila)
		dw_5.SelectRow(il_Fila, True)
		
	CASE 3
		lstr_mant.dw	=	dw_7
		SetNull(lstr_mant.dw2)
		lstr_mant.Argumento[4]	=	'2'
		OpenWithParm(iw_mantencion_2, lstr_mant)
		dw_7.SetRow(il_Fila)
		dw_7.SelectRow(il_Fila, True)
		
END CHOOSE

dw_6.SetFilter("")
dw_6.Filter()

Captura_Totales()
ProductoresLotes(lstr_mant.productores)
end event

event ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta, ll_fila_env, ll_row
Boolean	lb_habilita = TRUE

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]), integer(istr_mant.argumento[2]),&
										 Long(istr_mant.argumento[3]), Integer(istr_mant.argumento[10]))

	If ll_fila_e = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ElseIf ll_fila_e > 0 Then
		tab_1.tp_3.Enabled				=	True
		tab_1.tp_1.Enabled				=	True
		tab_1.tp_2.Enabled				=	True
		dw_2.Object.destare.Visible	=	True
		istr_Mant.Solo_Consulta			=	False
		
		istr_mant.Argumento[5]		= String(dw_2.Object.espe_codigo[1])
		istr_mant.Argumento[11]	= String(dw_2.Object.mfge_totbul[1])
		istr_mant.Argumento[8] 		= String(dw_2.Object.mfge_fecmov[1])
		
		If Not manbin_especie(dw_2.Object.plde_codigo[1], dw_2.Object.espe_codigo[1], True, sqlca) Then
			TriggerEvent("ue_nuevo")
			Return
		End If
		
		dw_2.SetRedraw(True)
		
		If Not gb_RecepcionDeProceso Then
			iuo_Camion.Existe(1, dw_2.Object.cami_patent[1], True, sqlca)
		End If
		
		HabilitaEncab(False)
		If iuo_especie.existe(dw_2.object.espe_codigo[1],TRUE,SQLCA) Then
			If iuo_especie.kildec = 1 Then
				ii_kildec = 2
			Else
				ii_kildec = 0
			End If	
		End If

		ll_fila_env	=	dw_4.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),&
											  Long(istr_mant.argumento[3]),1, Integer(istr_mant.argumento[10]))

		DO
			If dw_1.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]), Integer(istr_mant.argumento[10])) = -1 OR &
				dw_3.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),&
								  Long(istr_mant.argumento[3]),Integer(istr_mant.argumento[10])) = -1 OR &
				dw_6.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),Integer(istr_mant.argumento[10])) = -1 OR &
				dw_5.Retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),1, Integer(istr_mant.argumento[10])) = -1 OR &
				dw_7.Retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),2,Integer(istr_mant.argumento[10])) = -1 OR &
				dw_spro_bins.Retrieve(Long(istr_mant.argumento[1]),Long(istr_mant.argumento[3]),&
								  Integer(istr_mant.argumento[10])) = -1 OR &
				dw_9.Retrieve(Long(istr_mant.argumento[1]),Long(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3])) = -1 OR &
				dw_desverd.Retrieve(Integer(istr_mant.argumento[10]), Integer(istr_mant.argumento[1]),&
							     Integer(istr_mant.argumento[2]), Long(istr_mant.argumento[3])) = -1  Then
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			Else
				dw_pdf.Retrieve(Integer(istr_mant.argumento[10]), Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]), Long(istr_mant.argumento[3]))
				
				idwc_variedad.Retrieve(dw_2.Object.espe_codigo[1])
				
				//Para determinar Fuera de Rango
				Destare(False)
				
				FOR ll_Row = 1 TO dw_3.RowCount()
					 If Not VerIficaBultos(Integer(Istr_Mant.Argumento[1]), dw_3.Object.lote_pltcod[ll_row],&
										 		  dw_3.Object.lote_espcod[ll_row], dw_3.Object.lote_codigo[ll_row],&
										 		  dw_3.Object.lote_totbul[ll_row]) Then
						 								lb_Habilita = False
								 						ll_Row = dw_3.RowCount()
					End If
				NEXT

				If lb_Habilita Then
					Istr_Mant.Solo_Consulta = FALSE
				Else
					If Messagebox("Advertencia","Movimiento Solo De Consulta, ¿Desea ModIficar de todas formas?", Exclamation!, YesNo!, 2) = 1 Then
						Istr_Mant.Solo_Consulta = FALSE
					Else
						Istr_Mant.Solo_Consulta = TRUE
					End If
				End If

				dw_2.Enabled 			= Not istr_Mant.Solo_Consulta
				pb_eliminar.Enabled  = Not istr_Mant.Solo_Consulta
				pb_grabar.Enabled	= Not istr_Mant.Solo_Consulta
				pb_ins_det.Enabled	= Not istr_Mant.Solo_Consulta
				pb_imprimir.Enabled	= True
				pb_eli_det.Enabled	= Not istr_Mant.Solo_Consulta
				cb_guia.Enabled		= Not istr_Mant.Solo_Consulta
				
				il_fila = 1
				dw_3.SetRow(1)
				dw_3.SelectRow(1,True)
				dw_3.SetFocus()
			End If
		LOOP WHILE respuesta = 1

		If respuesta = 2 Then Close(This)
	End If
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

If respuesta = 2 Then Close(This)
end event

event ue_antesguardar;Date			ld_Fecha
Time			lt_hora
Boolean     lb_Actualiza_Fruta = FALSE, lb_Actualiza_Envase = FALSE
Integer 	 	li_TipoMovto, li_TipoMovtoEnva, li_Planta, li_pesaje, &
				li_Lote_pltcod, li_Lote_espcod, li_Lote_codigo, li_Secuencia, &
				li_TipoEnvase, li_Envase, li_Lote_Ant, li_camara, li_Cliente, li_pos
Long 			ll_Filas, ll_Total_Bultos, ll_Envases_Fruta, ll_Fila, ll_Fila_Busca, &
  				ll_Fila_d, ll_Primer_NumEnva, ll_Productor, ll_Numero
Decimal{3}	ld_Total_PesoEnv_Sal, ld_KBS_Camion, ld_KBS_Carro

Message.DoubleParm = 0

li_Planta			=	Integer(istr_mant.Argumento[1])
li_TipoMovto		=	Integer(istr_mant.Argumento[2])
li_TipoMovtoenva  =  41  // Recepción de Envases
li_Cliente			=	Integer(istr_mant.Argumento[10])

IF dw_4.RowCount() > 0 THEN
	il_numeroenva = dw_4.Object.meen_numero[1]
END IF
 
 FOR ll_Filas = dw_desverd.RowCount() TO 1 Step -1
	IF IsNull(dw_desverd.Object.ccag_codigo[ll_filas]) OR dw_desverd.Object.ccag_codigo[ll_filas] = 0 THEN
		dw_desverd.DeleteRow(ll_filas)
	END IF
NEXT

ll_Filas = dw_1.RowCount()

IF NOT chequea_BinsContraEnvases() THEN
	Message.DoubleParm = -1
	RETURN
END IF

IF dw_3.RowCount() > 0 THEN
	IF dw_3.Object.total_bultos[dw_3.RowCount()] >= 10000 THEN
		Messagebox("Error de Consistencia","El total de bultos supera lo permitido")
		Message.DoubleParm = -1
		RETURN
	ELSE
		ll_Total_Bultos	=	dw_3.Object.total_bultos[dw_3.RowCount()]
	END IF
END IF

IF dw_5.RowCount() > 0 THEN
	IF dw_5.Object.total_pesoenv[dw_5.RowCount()] >= 100000 THEN
		Messagebox("Error de Consistencia","El peso de los envases supera lo permitido")
		Message.DoubleParm = -1
		RETURN
	END IF
	
	IF dw_5.Object.total_envases[dw_5.RowCount()] >= 10000 THEN
		Messagebox("Error de Consistencia","El total de envases supera lo permitido")
		Message.DoubleParm = -1
		RETURN
	END IF
END IF

IF dw_5.RowCount() > 0 THEN
	ll_Envases_Fruta	=	dw_5.Object.tot_bultos_fruta[dw_5.RowCount()]
END IF

IF Chequea_EnvasesProductor() = False THEN
	Message.DoubleParm = -1
	RETURN
END IF

IF ib_Salida THEN
	IF Isnull(dw_2.Object.refg_tkbsal[1]) OR dw_2.Object.refg_tkbsal[1] = 0 THEN
		IF	MessageBox("Atención","No se ha registrado la Tara de Salida del Camión."+&
						" Desea Registrarla ?",Question!,YesNo!) = 1 THEN
			Message.DoubleParm = -1
			RETURN
		END IF
	END IF
	
	ll_Fila	=	dw_7.RowCount()

	IF ll_Fila > 0 THEN
		dw_7.GroupCalc()
		ld_Total_PesoEnv_Sal	=	dw_7.Object.total_pesoenv[ll_Fila]
	END IF

	ld_KBS_Camion	=	dw_2.Object.refg_tkbsal[1]
	ld_KBS_Carro	=	dw_2.Object.refg_tkbsac[1]

	IF ld_KBS_Camion + ld_KBS_Carro > 0 AND &
		ld_KBS_Camion + ld_KBS_Carro < ld_Total_PesoEnv_Sal THEN
		MessageBox("Atención","Kilos de Salida no pueden ser menor a los Envases de Salida")
		Message.DoubleParm = -1
		RETURN
	END IF
	
 	IF dw_2.Object.mfge_tpneto[1] >= 100000 THEN
		MessageBox("Atención","Los valores de la tara superan el calculo del Peso Neto permitido")
		Message.DoubleParm = -1
		RETURN
	END IF

	IF ib_Destare THEN
		//Pasa a Definitiva
		dw_2.Object.mfge_estmov[1]	=	3
		cb_guia.Enabled				=	True	
	ELSE
		MessageBox("Atención","Aún no realiza el destare, no puede grabar")
		Message.DoubleParm = -1
		RETURN
	END IF
	
END IF

IF Not gstr_paramplanta.packing  THEN
	IF dw_9.RowCount() <> dw_2.Object.mfge_totbul[1] AND (gstr_paramplanta.binsabins) THEN
		MessageBox("Atención","La cantidad de pesajes no corresponde a la cantidad de Bultos Recepcionados.")
		Message.DoubleParm = -1
		RETURN
	END IF
END IF

ib_AutoCommit		=	SQLCA.AutoCommit
SQLCA.AutoCommit	=	False

IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
  
  	IF il_NumEnva=0 THEN
		iuo_TipoMovtoEnva.bloqueacorrel()	
		il_NumEnva = iuo_TipoMovtoEnva.UltimoCorrelativo(4,li_TipoMovtoEnva,li_Planta) 
	
		IF il_NumEnva = 0 OR IsNull(il_NumEnva) THEN
			Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
			Message.DoubleParm = -1
			RETURN
		ELSE
			lb_Actualiza_Envase = TRUE
			ll_Primer_NumEnva   = il_NumEnva
		END IF
	ELSE
		ll_Primer_NumEnva   = il_NumEnva
	END IF
	
	IF il_NumFruta=0 THEN
		iuo_TipoMovtoFruta.bloqueacorrel()
		il_NumFruta = iuo_TipoMovtoFruta.UltimoCorrelativo(1,li_TipoMovto,li_Planta) 
	
		IF il_NumFruta = 0 OR IsNull(il_NumFruta) THEN
			Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
			Message.DoubleParm = -1
			RETURN
		ELSE
			lb_Actualiza_Fruta = TRUE	
		END IF
   END IF
   
	dw_2.Object.mfge_numero[1]	=	il_NumFruta

	Determina_ProductoresEnvase(li_TipoMovtoEnva)
	
	//Resetea Movto de Envase en caso de errores previos en la grabación.
	dw_4.Reset()
	
	FOR ll_Productor = 1 TO UpperBound(wstr_Prod_Enva.Productor)
		ll_Fila										=	dw_4.InsertRow(0)
		dw_4.Object.plde_codigo[ll_Fila]		=	li_Planta
		dw_4.Object.tpmv_codigo[ll_Fila]		=	li_TipoMovtoEnva
		dw_4.Object.meen_numero[ll_Fila]		=	il_NumEnva
		dw_4.Object.tpmv_codrec[ll_Fila]		=	dw_2.Object.tpmv_codigo[1]
		dw_4.Object.mfge_numero[ll_Fila]		=	il_NumFruta
		dw_4.Object.meen_modulo[ll_Fila]		=	1
		dw_4.Object.plde_coorde[ll_Fila]		=	dw_2.Object.plde_coorde[1]
		dw_4.Object.prod_codigo[ll_Fila]		=	Long(wstr_Prod_Enva.Productor[ll_Productor])
      dw_4.Object.clie_codigo[ll_Fila]		=	li_Cliente
		dw_4.Object.meen_guisii[ll_Fila]		=	wstr_Prod_Enva.GuiaSII[ll_Productor]
		dw_4.Object.meen_fecmov[ll_Fila]		=	dw_2.Object.mfge_fecmov[1]
		dw_4.Object.tran_codigo[ll_Fila]		=	dw_2.Object.tran_codigo[1]
		dw_4.Object.cami_clasifi[ll_Fila]	=	dw_2.Object.cami_clasifi[1]
		dw_4.Object.cami_patent[ll_Fila]		=	dw_2.Object.cami_patent[1]
		dw_4.Object.cami_patcar[ll_Fila]		=	dw_2.Object.cami_patcar[1]
		dw_4.Object.meen_rutcho[ll_Fila]		=	dw_2.Object.mfge_rutcho[1]
		dw_4.Object.meen_chofer[ll_Fila]		=	dw_2.Object.mfge_chofer[1]
		il_NumEnva									++
		
	NEXT
	//Descuenta último Correlativo Envases acumulado.
   il_NumEnva 										--
	//Preguntar el Momento de Actualización
	IF lb_Actualiza_Fruta  THEN iuo_TipoMovtoFruta.Actualiza_Correlativo(1,li_Planta,li_TipoMovto,il_NumFruta) 
   IF lb_Actualiza_Envase THEN iuo_TipoMovtoEnva.Actualiza_Correlativo(4,li_Planta,li_TipoMovtoEnva,il_NumEnva) 
   ///////////////////////////////////////
	il_NumEnva = ll_Primer_NumEnva
	
ELSE
	il_NumFruta	=	dw_2.Object.mfge_numero[1]
	
	Determina_ProductoresEnvase(li_TipoMovtoEnva)
	FOR ll_Productor = 1 TO UpperBound(wstr_Prod_Enva.Productor)
		li_Pos = dw_4.Find('plde_codigo = ' + String(li_Planta)  + ' And tpmv_codigo = ' + String(li_TipoMovtoEnva) + &
						  ' And clie_codigo = ' + String(li_Cliente) + ' And meen_numero = ' + String(wstr_Prod_Enva.Numero[ll_Productor]), 1, dw_4.RowCount())
		If li_pos > 0 Then
			dw_4.Object.prod_codigo[li_Pos]	=	wstr_Prod_Enva.Productor[ll_Productor]
			dw_4.Object.meen_guisii[li_Pos]	=	wstr_Prod_Enva.GuiaSII[ll_Productor]
		End If
	NEXT
END IF

istr_mant.Argumento[3]	=	String(il_NumFruta)

SELECT	IsNull(Max(mfgd_secuen), 0) + 1
	INTO	:li_Secuencia
	FROM	dbo.spro_movtofrutagrandeta
	WHERE	plde_codigo	=	:li_Planta
	AND	tpmv_codigo	=	:li_TipoMovto
	AND	mfge_numero	=	:il_NumFruta
	AND   clie_codigo =  :li_Cliente;
	
ll_filas = dw_1.RowCount()	

FOR ll_Fila = 1 TO dw_6.RowCount()
	
	li_Lote_pltcod	=	dw_6.Object.lote_pltcod[ll_Fila]
	li_Lote_espcod	=	dw_6.Object.lote_espcod[ll_Fila]
	li_Lote_codigo	=	dw_6.Object.lote_codigo[ll_Fila]
	li_TipoEnvase	=	dw_6.Object.enva_tipoen[ll_Fila]
	li_Envase		=	dw_6.Object.enva_codigo[ll_Fila]

	IF li_Lote_Codigo <> li_Lote_Ant THEN
		ll_Fila_Busca	=	dw_3.Find("lote_pltcod = "+String(li_Lote_pltcod)+" and "+&
											 "lote_espcod = "+String(li_Lote_espcod)+" and "+&
											 "lote_codigo = "+String(li_Lote_codigo),1,dw_3.RowCount())
		IF ll_Fila_Busca > 0 THEN
			dw_3.Object.fgcc_fecrec[ll_Fila_Busca]	=	dw_2.Object.mfge_fecmov[1]
			li_Camara	=	dw_3.Object.cama_codigo[ll_Fila_Busca]
		END IF
		li_Lote_Ant	=	li_Lote_codigo
	END IF
	
	ll_Fila_d	=	dw_1.Find("lote_pltcod = "+String(li_Lote_pltcod)+" and "+&
									 "clie_codigo = "+String(li_cliente)+" and "+&
									 "lote_espcod = "+String(li_Lote_espcod)+" and "+&
									 "lote_codigo = "+String(li_Lote_codigo)+" and "+&
									 "enva_tipoen = "+String(li_TipoEnvase)+" and "+&
									 "enva_codigo = "+String(li_Envase),1,dw_1.RowCount())
	IF ll_Fila_d = 0 THEN
		ll_Fila_d	=	dw_1.InsertRow(0)
		dw_1.Object.plde_codigo[ll_Fila_d]	=	li_Planta
		dw_1.Object.tpmv_codigo[ll_Fila_d]	=	dw_2.Object.tpmv_codigo[1]
		dw_1.Object.mfge_numero[ll_Fila_d]	=	dw_2.Object.mfge_numero[1]
		dw_1.Object.mfgd_secuen[ll_Fila_d]	=	li_Secuencia
		li_Secuencia ++
	END IF

	dw_1.Object.plde_coorde[ll_Fila_d]	=	li_Planta
	dw_1.Object.cama_codigo[ll_Fila_d]	=	li_Camara
	dw_1.Object.lote_pltcod[ll_Fila_d]	=	li_Lote_pltcod
	dw_1.Object.lote_espcod[ll_Fila_d]	=	li_Lote_espcod
	dw_1.Object.lote_codigo[ll_Fila_d]	=	li_Lote_codigo
	dw_1.Object.enva_tipoen[ll_Fila_d]	=	li_TipoEnvase
	dw_1.Object.enva_codigo[ll_Fila_d]	=	li_Envase
	dw_1.Object.mfgd_bulent[ll_Fila_d]	=	dw_6.Object.lotd_totbul[ll_Fila]
	dw_1.Object.mfgd_kgnent[ll_Fila_d]	=	dw_6.Object.lotd_totnet[ll_Fila]
	dw_1.Object.clie_codigo[ll_Fila_d]	=	li_Cliente	
NEXT

//Elimina Filas de Detalle de Movimiento que fueron eliminadas de Lotes
ll_Fila	= 1

DO WHILE ll_Fila <= dw_1.RowCount()
	
	li_Lote_pltcod	=	dw_1.Object.lote_pltcod[ll_Fila]
	li_Lote_espcod	=	dw_1.Object.lote_espcod[ll_Fila]
	li_Lote_codigo	=	dw_1.Object.lote_codigo[ll_Fila]
	li_TipoEnvase	=	dw_1.Object.enva_tipoen[ll_Fila]
	li_Envase		=	dw_1.Object.enva_codigo[ll_Fila]

	ll_Fila_d	=	dw_6.Find("lote_pltcod = "	+	String(li_Lote_pltcod)	+	" and "+&
	                                  "lote_espcod = "	+	String(li_Lote_espcod)	+	" and "+&
 								  "lote_codigo = "	+	String(li_Lote_codigo)	+	" and "+&
								  "enva_tipoen = "	+	String(li_TipoEnvase)	+	" and "+&
								  "enva_codigo = "	+	String(li_Envase),1,dw_6.RowCount())

	IF ll_Fila_d = 0 THEN
		dw_1.DeleteRow(ll_Fila)
	ELSE
		ll_Fila ++
	END IF
LOOP

FOR ll_Fila = 1 TO dw_5.RowCount()
	
	IF dw_5.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		ll_Productor	=	dw_5.Object.prod_codigo[ll_Fila]
		
		ll_Fila_Busca	=	dw_4.Find("plde_codigo = "+String(li_Planta) + " and " + &
											 "tpmv_codigo = "+String(li_TipoMovtoEnva) + " and " + &
											 "clie_codigo = "+String(li_Cliente) + " and " + &											 
											 "prod_codigo = "+String(ll_Productor),1,dw_4.RowCount())
		
		IF ll_Fila_Busca > 0 THEN
												 
			dw_5.Object.plde_codigo[ll_Fila]	=	dw_4.Object.plde_codigo[ll_Fila_Busca]
			dw_5.Object.tpmv_codigo[ll_Fila]	=	dw_4.Object.tpmv_codigo[ll_Fila_Busca]
			dw_5.Object.meen_numero[ll_Fila]	=	dw_4.Object.meen_numero[ll_Fila_Busca]
			dw_5.Object.clie_codigo[ll_Fila]	=	li_Cliente
		END IF
	END IF
NEXT

FOR ll_Fila = 1 TO dw_7.RowCount()
	
	IF dw_7.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		ll_Productor	=	dw_7.Object.prod_codigo[ll_Fila]
		
		ll_Fila_Busca	=	dw_4.Find("plde_codigo = "+String(li_Planta)+" and " + &
											 "tpmv_codigo = 41 and " + &
											 "clie_codigo = "+String(li_Cliente) + " and " + &											 
											 "prod_codigo = "+String(ll_Productor),1,dw_4.RowCount())
		
		IF ll_Fila_Busca > 0 THEN
												 
			dw_7.Object.plde_codigo[ll_Fila]	=	dw_4.Object.plde_codigo[ll_Fila_Busca]
			dw_7.Object.tpmv_codigo[ll_Fila]	=	dw_4.Object.tpmv_codigo[ll_Fila_Busca]
			dw_7.Object.meen_numero[ll_Fila]	=	dw_4.Object.meen_numero[ll_Fila_Busca]
			dw_7.Object.clie_codigo[ll_Fila]	=	li_Cliente			
		END IF
	END IF
NEXT
/*Selecciona secuencia para tabla de pesajes*/

ll_numero	=	dw_2.Object.mfge_numero[1]
li_Pesaje	=	0
 dw_9.SetSort("mfgp_nropes asc")
 dw_9.Sort()
 
FOR ll_Fila = 1 TO dw_9.RowCount()
	IF dw_9.Object.mfgp_nropes[ll_Fila] <> li_Pesaje THEN
		li_Pesaje	=	dw_9.Object.mfgp_nropes[ll_Fila]
		
		SELECT	IsNull(Max(mfgp_secuen),0) + 1
		  INTO	:li_Secuencia
		  FROM	dbo.spro_movtofrutagranpesa
		 WHERE	plde_codigo	=	:li_Planta
			AND	tpmv_codigo =	:li_TipoMovto
			AND	mfge_numero =	:ll_Numero
			AND	mfgp_nropes	=	:li_Pesaje ;
	END IF
	
	IF dw_9.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_9.Object.clie_codigo[ll_fila] 	= 	dw_2.Object.clie_codigo[1]
		dw_9.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_9.Object.tpmv_codigo[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
		dw_9.Object.mfge_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]		
		dw_9.Object.mfgp_secuen[ll_Fila]	=	li_secuencia		
		li_secuencia ++
	END IF
NEXT

FOR ll_Fila = 1 TO dw_spro_bins.RowCount()
//	IF dw_spro_bins.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		dw_spro_bins.Object.mfge_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]
		dw_spro_bins.Object.lote_pltcod[ll_Fila]	=	li_Lote_pltcod
//	END IF
NEXT

//FOR ll_fila = 1 TO dw_desverd.RowCount()
//	dw_desverd.Object.lode_estado[ll_fila]	=	1
//	
//NEXT

//SETEA CLIENTE = CLIE_CODIGO SELECCIONADO
//Repara falla para proceso 3ros.
FOR ll_fila = 1 TO dw_1.Rowcount()
	dw_1.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[10])
NEXT

FOR ll_fila = 1 TO dw_3.Rowcount()
	dw_3.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[10])
NEXT

FOR ll_fila = 1 TO dw_4.Rowcount()
	dw_4.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[10])
NEXT

FOR ll_fila = 1 TO dw_5.Rowcount()
	dw_5.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[10])
NEXT

FOR ll_fila = 1 TO dw_7.Rowcount()
	dw_7.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[10])
NEXT

dw_6.RowsDiscard (1, dw_6.Deletedcount(), Delete!) 
FOR ll_Filas = 1 TO dw_6.RowCount()
	 dw_6.SetItemStatus(ll_Filas, 0, Primary!, Tipo_Update_Lote(dw_6.Object.lote_pltcod[ll_filas], &
																				  dw_6.Object.lote_espcod[ll_filas], &
																				  dw_6.Object.lote_codigo[ll_filas], &
																				  dw_6.Object.enva_tipoen[ll_filas], &
																				  dw_6.Object.enva_codigo[ll_filas]))
NEXT


end event

event ue_seleccion;call super::ue_seleccion;IF ib_ok = False THEN RETURN

Str_Busqueda	lstr_busq

lstr_busq.argum[1] = String(gstr_param.plde_codigo)
lstr_busq.argum[2] = String(ii_TipMov)							// Movimiento de Recepción
lstr_busq.argum[3] = '3' 												// Sólo Definitivas
lstr_busq.argum[4] = String(idt_FechaSistema)   				// Desde Fecha de Inicio Ducha
lstr_busq.argum[10] = String(dw_2.Object.clie_codigo[1]) 	// cliente

OpenWithParm(w_busc_spro_movtofrutagranenca_recepcion, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	istr_mant.Argumento[1]	=	lstr_busq.argum[1]
	istr_mant.Argumento[2]	=	lstr_busq.argum[2]
	istr_mant.argumento[3]	=	lstr_busq.argum[3]
	istr_mant.argumento[10]	=	lstr_busq.argum[10]
	
	This.TriggerEvent("ue_recuperadatos")
END IF
end event

event ue_imprimir;Long	ll_modIf
Date	ld_FechaRecepcion

If Not istr_mant.Solo_Consulta Then
	CHOOSE CASE wf_modIfica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modIf	=	dw_1.GetNextModIfied(0, Primary!)
			ll_modIf	+=	dw_2.GetNextModIfied(0, Primary!)
			ll_modIf	+=	dw_3.GetNextModIfied(0, Primary!)
			ll_modIf	+=	dw_6.GetNextModIfied(0, Primary!)
			ll_modIf	+=	dw_5.GetNextModIfied(0, Primary!)
			ll_modIf	+=	dw_7.GetNextModIfied(0, Primary!)
		
			If dw_3.RowCount() > 0 and ll_modIf > 0 Then
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información antes de Imprimir ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						If message.DoubleParm = -1 Then ib_ok = False
					CASE 3
						ib_ok	= False
						RETURN
				End CHOOSE
			End If
	End CHOOSE
End If

If Not ib_ok Then RETURN

SetPointer(HourGlass!)

Long		fila
Integer  li_estado, li_Kilos

istr_info.titulo	= "GUIA DE RECEPCION FRUTA GRANEL"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

li_estado	=	dw_2.object.mfge_estmov[1]

If li_estado=1 Then
   vinf.dw_1.DataObject = "dw_info_guia_recepcion_Transitoria"
Else
	vinf.dw_1.DataObject = "dw_info_guia_recepcion_Definitiva"
	li_Kilos	=	MessageBox("Emisión Definitiva","Desea emitir Guía de Recepción con Kilos",Question!,YesNo!,1)
	If li_Kilos = 2 Then li_Kilos = 0
End If

ld_FechaRecepcion	=	dw_2.object.mfge_fecmov[1]

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]),Integer(istr_mant.Argumento[2]),Integer(istr_mant.Argumento[3]),&
								  Integer(istr_mant.Argumento[10]), ld_FechaRecepcion, ld_FechaRecepcion)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

event closequery; IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			Message.ReturnValue = 1 
		CASE 0
			IF dw_3.RowCount() > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.triggerevent("ue_guardar")
						IF message.doubleparm = -1 THEN Message.ReturnValue = 1
						RETURN
						
					CASE 3
						Message.ReturnValue = 1
						RETURN
						
				END CHOOSE
			END IF
	END CHOOSE
END IF
end event

event ue_validaborrar;Long ll_fila
IF MessageBox("Borrar Registro","Desea Borrar la Información ?", Question!, YesNo!) = 1 THEN
	Message.DoubleParm = 1
	
	FOR ll_fila = 1 TO dw_3.RowCount()
		IF NOT VerificaLote(dw_3.Object.lote_pltcod[ll_fila],&
									dw_3.Object.lote_espcod[ll_fila],&
									dw_3.Object.lote_codigo[ll_fila]) THEN
			Message.DoubleParm = -1
			RETURN
		END IF	
	NEXT	
ELSE
	Message.DoubleParm = -1
END IF

RETURN
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtofrutagranel_mantrecepcion
boolean visible = false
integer x = 155
integer y = 1356
integer width = 3913
integer height = 1036
string title = "Detalle de Movimientos"
string dataobject = "dw_mues_movtofrutagraneldeta_despacho"
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtofrutagranel_mantrecepcion
integer x = 82
integer y = 20
integer width = 3269
integer height = 824
integer taborder = 130
string dataobject = "dw_mant_movtofrutagranenca"
boolean livescroll = true
end type

event dw_2::itemchanged;String	ls_Nula
String	ls_Columna
Date		ldt_Fecha

ls_Columna = GetColumnName()
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(row,"clie_codigo",Integer(ls_Nula))
			RETURN 1
		ELSE
			istr_mant.Argumento[10] = data
		END IF		
		
	CASE "mfge_numero"
		IF NOT ExisteRecepcion(gstr_ParamPlanta.CodigoPlanta, ii_TipMov, Integer(data),dw_2.object.clie_codigo[1]) THEN
			This.SetItem(1,"mfge_numero",Long(ls_Nula))
			This.SetFocus()
			RETURN 1
		END IF
	
	CASE "refg_tkbent", "refg_tkbenc", "refg_tkbsal", "refg_tkbsac"
		IF Not This.uf_validate(row) THEN
			This.SetItem(row,ls_Columna,Dec(ls_Nula))
			RETURN 1
		ELSE
			captura_Totales()
		END IF

END CHOOSE
end event

event dw_2::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name
	CASE "destare"
		IF Not ib_Salida THEN
			MessageBox("Advertencia","Las modificaciones a los valores ingresados, ~r~rpueden ocasionar cambios en los resultados.")
			IF Not gstr_paramplanta.packing  THEN AsignaPeso()
			HabilitaSalida()
		ELSE
			captura_totales()
			IF Not gstr_paramplanta.packing  THEN AsignaPeso()
			Destare(True)
			IF gstr_paramplanta.bultobins THEN
				UpdateMovtoGranPesa()
			END IF
		END IF
		
END CHOOSE
end event

event dw_2::itemfocuschanged;call super::itemfocuschanged;IF is_rut <> "" THEN
	This.Object.mfge_rutcho.Format = '@@@.@@@.@@@-@'
	
	IF dwo.Name <> "mfge_rutcho" THEN
		This.SetItem(1, "mfge_rutcho", is_rut)
	END IF
END IF
end event

event dw_2::constructor;call super::constructor;This.uf_add_validation( 'refg_tkbsal >= 0 and refg_tkbsal < 99999.999','Valor fuera de rango')
This.uf_add_validation( 'refg_tkbsac >= 0 and refg_tkbsac < 99999.999','Valor fuera de rango')
This.uf_add_validation( 'refg_tkbent >= 0 and refg_tkbent < 99999.999','Valor fuera de rango')
This.uf_add_validation( 'refg_tkbenc >= 0 and refg_tkbenc < 99999.999','Valor fuera de rango')
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtofrutagranel_mantrecepcion
integer x = 4434
integer y = 260
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtofrutagranel_mantrecepcion
integer x = 4434
integer y = 436
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtofrutagranel_mantrecepcion
integer x = 4434
integer y = 620
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtofrutagranel_mantrecepcion
integer x = 4434
integer y = 800
integer taborder = 60
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtofrutagranel_mantrecepcion
integer x = 4439
integer y = 1024
integer taborder = 100
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtofrutagranel_mantrecepcion
integer x = 4434
integer y = 1452
integer taborder = 180
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtofrutagranel_mantrecepcion
integer x = 4434
integer y = 1728
integer taborder = 210
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtofrutagranel_mantrecepcion
integer x = 4434
integer y = 76
end type

type dw_4 from datawindow within w_maed_movtofrutagranel_mantrecepcion
boolean visible = false
integer x = 3835
integer y = 440
integer width = 219
integer height = 168
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_movtoenvaenca_recepfruta"
borderstyle borderstyle = stylelowered!
end type

type cb_guia from commandbutton within w_maed_movtofrutagranel_mantrecepcion
integer x = 4434
integer y = 1272
integer width = 302
integer height = 92
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Guía SII"
end type

event clicked;Long		ll_Fila, ll_Fila_Busca,li_Productor
	

str_mant	lstr_mant

Determina_ProductoresEnvase(61)

FOR ll_Fila = 1 TO UpperBound(wstr_Prod_Enva.Productor)
	li_Productor	=	wstr_Prod_Enva.Productor[ll_Fila]
	
	ll_Fila_Busca	=	dw_4.Find("clie_codigo = "+istr_Mant.Argumento[10]+" and "+&
	                            "plde_codigo = "+istr_Mant.Argumento[1]+" and "+&
										 "tpmv_codigo = 41 and "+&
										 "prod_codigo = "+String(li_Productor),1,dw_4.RowCount())
		
	IF ll_Fila_Busca > 0 THEN
												 
		lstr_Mant.Argumento[1]	=	String(dw_4.Object.plde_codigo[ll_Fila_Busca])
		lstr_Mant.Argumento[2]	=	String(dw_4.Object.tpmv_codigo[ll_Fila_Busca])
		lstr_Mant.Argumento[3]	=	String(dw_4.Object.meen_numero[ll_Fila_Busca])
		lstr_mant.Argumento[5]  =  String(dw_4.Object.clie_codigo[ll_Fila_Busca])
		
		OpenWithParm(w_emis_guia_despacho_envases, lstr_Mant)
	END IF
NEXT


end event

type dw_9 from datawindow within w_maed_movtofrutagranel_mantrecepcion
boolean visible = false
integer x = 3835
integer y = 268
integer width = 219
integer height = 168
integer taborder = 190
boolean bringtotop = true
string dataobject = "dw_pesaje_romana"
borderstyle borderstyle = stylelowered!
end type

type dw_exidetaborra from datawindow within w_maed_movtofrutagranel_mantrecepcion
boolean visible = false
integer x = 965
integer y = 1848
integer width = 686
integer height = 400
integer taborder = 200
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_exismovtodeta_borra"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exismovtodetanulos from datawindow within w_maed_movtofrutagranel_mantrecepcion
boolean visible = false
integer x = 2427
integer y = 1856
integer width = 686
integer height = 400
integer taborder = 150
boolean bringtotop = true
string title = "none"
string dataobject = "dw_inserta_exismovtodetanulos"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exideta from datawindow within w_maed_movtofrutagranel_mantrecepcion
boolean visible = false
integer x = 3095
integer y = 1828
integer width = 686
integer height = 400
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_exismovtodeta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event dberror;String	ls_Tipo, ls_Mensaje

Str_ErrorBaseDatos	lstr_ErrBD

CHOOSE CASE buffer
	CASE delete!
		ls_Tipo = "Borrando"
		
	CASE primary!
		DwItemStatus Stat
		
		Stat	=	This.getitemstatus(Row, 0, Buffer)
		
		IF Stat = New! OR Stat = NewModified! THEN
			ls_Tipo	=	"Agregando"
		ELSE
			ls_Tipo	=	"Actualizando"
		END IF
		
END CHOOSE

lstr_ErrBD.Titulo	=	"Error " + ls_Tipo + " registro " + String(row)
lstr_ErrBD.Numero	=	SqlDbCode
lstr_ErrBD.Texto	=	SqlErrText
is_error				=  SqlErrText
il_coderror			=	SqlDbCode

ls_Mensaje	=	"Error " + ls_Tipo + " registro " + String(row)
ls_Mensaje	+=	"~r~nNúmero de Error Base Datos: " + String(SqlDbCode)
ls_Mensaje	+=	"~r~nMensaje de Error Base Datos:~r~n~r~n" + SqlErrText

lstr_ErrBD.MensajePantalla	=	ls_Mensaje

//OpenWithParm(w_ErrorBaseDatos, lstr_ErrBD)

This.SetFocus()
This.SetRow(row)
This.ScrollToRow(row)

RETURN 1
end event

type dw_exiencab from datawindow within w_maed_movtofrutagranel_mantrecepcion
boolean visible = false
integer x = 1696
integer y = 1856
integer width = 686
integer height = 400
integer taborder = 160
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_exismovtoenca_analisis"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event dberror;String	ls_Tipo, ls_Mensaje

Str_ErrorBaseDatos	lstr_ErrBD

CHOOSE CASE buffer
	CASE delete!
		ls_Tipo = "Borrando"
		
	CASE primary!
		DwItemStatus Stat
		
		Stat	=	This.getitemstatus(Row, 0, Buffer)
		
		IF Stat = New! OR Stat = NewModified! THEN
			ls_Tipo	=	"Agregando"
		ELSE
			ls_Tipo	=	"Actualizando"
		END IF
		
END CHOOSE

lstr_ErrBD.Titulo	=	"Error " + ls_Tipo + " registro " + String(row)
lstr_ErrBD.Numero	=	SqlDbCode
lstr_ErrBD.Texto	=	SqlErrText
is_error				=  SqlErrText
il_coderror			=	SqlDbCode

ls_Mensaje	=	"Error " + ls_Tipo + " registro " + String(row)
ls_Mensaje	+=	"~r~nNúmero de Error Base Datos: " + String(SqlDbCode)
ls_Mensaje	+=	"~r~nMensaje de Error Base Datos:~r~n~r~n" + SqlErrText

lstr_ErrBD.MensajePantalla	=	ls_Mensaje

//OpenWithParm(w_ErrorBaseDatos, lstr_ErrBD)

This.SetFocus()
This.SetRow(row)
This.ScrollToRow(row)

RETURN 1
end event

type dw_envases_comer from datawindow within w_maed_movtofrutagranel_mantrecepcion
boolean visible = false
integer y = 1804
integer width = 686
integer height = 400
integer taborder = 170
boolean bringtotop = true
string title = "none"
string dataobject = "dw_detalle_envases_comercial"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_lotescategoria from datawindow within w_maed_movtofrutagranel_mantrecepcion
boolean visible = false
integer y = 1940
integer width = 686
integer height = 400
integer taborder = 220
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_lotesfrutagranel_recepcion"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_pdf from uo_dw within w_maed_movtofrutagranel_mantrecepcion
boolean visible = false
integer x = 4087
integer y = 68
integer width = 219
integer height = 168
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_recepcionhuertopdf"
boolean vscrollbar = false
end type

type dw_docto from uo_dw within w_maed_movtofrutagranel_mantrecepcion
boolean visible = false
integer x = 3835
integer y = 64
integer width = 219
integer height = 168
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_info_guia_recepcion_Definitiva"
boolean vscrollbar = false
end type

type dw_desverd from datawindow within w_maed_movtofrutagranel_mantrecepcion
boolean visible = false
integer x = 4087
integer y = 456
integer width = 219
integer height = 152
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_spro_lotesfrutagranel_desverd_corto"
borderstyle borderstyle = stylelowered!
end type

type dw_spro_bins from datawindow within w_maed_movtofrutagranel_mantrecepcion
boolean visible = false
integer x = 759
integer y = 12
integer width = 2866
integer height = 612
integer taborder = 80
boolean titlebar = true
string dataobject = "dw_spro_movtobins"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type ole_puerta from olecustomcontrol within w_maed_movtofrutagranel_mantrecepcion
event oncomm ( )
boolean visible = false
integer x = 3643
integer y = 68
integer width = 174
integer height = 152
integer taborder = 30
boolean bringtotop = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
string binarykey = "w_maed_movtofrutagranel_mantrecepcion.win"
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type

type tab_1 from tab within w_maed_movtofrutagranel_mantrecepcion
integer x = 105
integer y = 992
integer width = 4082
integer height = 1044
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean fixedwidth = true
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
integer selectedtab = 1
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
end type

on tab_1.create
this.tp_1=create tp_1
this.tp_2=create tp_2
this.tp_3=create tp_3
this.Control[]={this.tp_1,&
this.tp_2,&
this.tp_3}
end on

on tab_1.destroy
destroy(this.tp_1)
destroy(this.tp_2)
destroy(this.tp_3)
end on

type tp_1 from userobject within tab_1
string tag = "Detalle de Lotes Ingresados"
integer x = 18
integer y = 112
integer width = 4046
integer height = 916
boolean enabled = false
long backcolor = 16777215
string text = "Lotes Recepcionados"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "DatabaseProfile5!"
long picturemaskcolor = 536870912
dw_lotes dw_lotes
end type

on tp_1.create
this.dw_lotes=create dw_lotes
this.Control[]={this.dw_lotes}
end on

on tp_1.destroy
destroy(this.dw_lotes)
end on

type dw_lotes from uo_dw within tp_1
integer x = 18
integer y = 32
integer width = 3991
integer height = 840
integer taborder = 10
string dataobject = "dw_mues_spro_lotesfrutagranel_recepcion"
boolean hscrollbar = true
boolean livescroll = true
end type

event doubleclicked;call super::doubleclicked;w_maed_movtofrutagranel_mantrecepcion.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event dwnkey;call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutagranel_mantrecepcion.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_mantrecepcion.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

type tp_2 from userobject within tab_1
string tag = "Registro de Envases recepcionados con y sin fruta"
integer x = 18
integer y = 112
integer width = 4046
integer height = 916
boolean enabled = false
long backcolor = 16777215
string text = "Envases Recibidos"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "CreateRuntime!"
long picturemaskcolor = 536870912
dw_envrec dw_envrec
end type

on tp_2.create
this.dw_envrec=create dw_envrec
this.Control[]={this.dw_envrec}
end on

on tp_2.destroy
destroy(this.dw_envrec)
end on

type dw_envrec from uo_dw within tp_2
integer x = 27
integer y = 36
integer width = 3991
integer height = 840
integer taborder = 11
string dataobject = "dw_mues_movtoenvadeta_recepfruta"
boolean hscrollbar = true
boolean livescroll = true
end type

event dwnkey;call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_mantrecepcion.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event doubleclicked;call super::doubleclicked;w_maed_movtofrutagranel_mantrecepcion.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

type tp_3 from userobject within tab_1
string tag = "Registro de Envases que retira para cosecha"
integer x = 18
integer y = 112
integer width = 4046
integer height = 916
boolean enabled = false
long backcolor = 16777215
string text = "Envases Retirados"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Inherit!"
long picturemaskcolor = 536870912
dw_envdes dw_envdes
end type

on tp_3.create
this.dw_envdes=create dw_envdes
this.Control[]={this.dw_envdes}
end on

on tp_3.destroy
destroy(this.dw_envdes)
end on

type dw_envdes from uo_dw within tp_3
integer x = 27
integer y = 36
integer width = 3991
integer height = 840
integer taborder = 21
string dataobject = "dw_mues_movtoenvadeta_recepfruta"
boolean hscrollbar = true
boolean livescroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event dwnkey;call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_mantrecepcion.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event doubleclicked;call super::doubleclicked;w_maed_movtofrutagranel_mantrecepcion.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

type dw_6 from datawindow within w_maed_movtofrutagranel_mantrecepcion
boolean visible = false
integer x = 1861
integer y = 820
integer width = 2427
integer height = 664
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_lotesfrutagradet_recepcion"
borderstyle borderstyle = stylelowered!
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
0Aw_maed_movtofrutagranel_mantrecepcion.bin 
2B00000600e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe00000006000000000000000000000001000000010000000000001000fffffffe00000000fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1Aw_maed_movtofrutagranel_mantrecepcion.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
