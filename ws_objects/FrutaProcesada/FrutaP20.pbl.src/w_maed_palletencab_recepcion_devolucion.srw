$PBExportHeader$w_maed_palletencab_recepcion_devolucion.srw
forward
global type w_maed_palletencab_recepcion_devolucion from w_maed_palletencab_trans
end type
type dw_3 from datawindow within w_maed_palletencab_recepcion_devolucion
end type
end forward

global type w_maed_palletencab_recepcion_devolucion from w_maed_palletencab_trans
integer width = 3954
integer height = 1964
dw_3 dw_3
end type
global w_maed_palletencab_recepcion_devolucion w_maed_palletencab_recepcion_devolucion

type variables
Str_mant	istr_mant2

DataWindowChild	idwc_categorias, idwc_status, idwc_condicion, &
						idwc_tratamiento, idwc_tipofrio, idwc_destino
										  
Boolean		lb_MensPallet=False, lb_MensPucho=False
Integer		ii_CantPallets, ii_CantPuchos;

/*================================================================
Vector que guarda datos sobre la ubicacion de un pallet, el vector 
se compondrá por la sgte asignacion:
i_vec_ubicacion[1]=camara, i_vec_ubicacion[2]=calle, 
i_vec_ubicacion[3]=base,   i_vec_ubicacion[4]=posicion,
i_vec_ubicacion[5]=tipo camara
================================================================*/
integer 		i_vec_ubicacion[5] 
Boolean	lb_TerminaPallet, lb_terminaPucho
Long		il_pallet


end variables

forward prototypes
public subroutine elimina_recprefriopda (integer li_clie_codigo, integer li_plde_codigo, long ll_paen_numero)
public function boolean existe_recprefriopda (integer li_clie_codigo, integer li_plde_codigo, long ll_paen_numero)
public subroutine existeotraplanta (long al_pallet)
public function boolean existepallet (string ls_columna)
public function boolean grabadocrel ()
public function boolean noesnuevo (integer planta, integer cliente, long numero)
public function boolean noexistetarja (integer planta, integer folio, integer cliente, long pallet)
public function boolean noexistetarjare (integer planta, integer folio, integer cliente, long pallet)
public function integer palletsinencab (integer planta, integer cliente, long numero)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean existepallet_definitivo (string ls_columna)
public function boolean existe_pallet_devolucion (string ls_columna)
public function boolean existecapturapallet (integer ai_cliente, integer ai_planta, long al_pallet)
public function boolean duplicado (integer ai_cliente, long al_numero)
end prototypes

public subroutine elimina_recprefriopda (integer li_clie_codigo, integer li_plde_codigo, long ll_paen_numero);//
DELETE FROM dbo.recprefriopda WHERE
	clie_codigo= :li_clie_codigo AND 
	plde_codigo= :li_plde_codigo AND
	paen_numero= :ll_paen_numero;
	
	/*control SQl*/


end subroutine

public function boolean existe_recprefriopda (integer li_clie_codigo, integer li_plde_codigo, long ll_paen_numero);Integer	li_Nulo

SetNull(li_Nulo)

i_vec_ubicacion[1]	=	li_Nulo
i_vec_ubicacion[2]	=	li_Nulo
i_vec_ubicacion[3]	=	li_Nulo
i_vec_ubicacion[4]	=	li_Nulo
i_vec_ubicacion[5]	=	li_Nulo

Select CAMA_CODIGO,PAEN_CALLE,PAEN_BASE,PAEN_POSICI INTO 
	:i_vec_ubicacion[1],:i_vec_ubicacion[2],:i_vec_ubicacion[3],:i_vec_ubicacion[4] FROM 
	dbo.PALLETENCAB_trans WHERE
	clie_codigo= :li_clie_codigo AND 
	plde_codigo= :li_plde_codigo AND
	paen_numero= :ll_paen_numero;
	/*control SQl*/
	
	
//verifica si encontró registro en la dbo.RECPREFRIOPDA
if isnull(i_vec_ubicacion[1]) then
	return FALSE
else
	
	Select cama_tipoca into 
	:i_vec_ubicacion[5] from
	dbo.camarasbode where
	plde_codigo= :li_plde_codigo AND
	cama_codigo= :i_vec_ubicacion[1];
	/*control SQl*/
	return TRUE
end if	
	
end function

public subroutine existeotraplanta (long al_pallet);Integer	li_Cliente, li_Planta, li_Estado

li_Cliente	=	dw_2.Object.clie_codigo[1]
li_Planta	=	dw_2.Object.plde_codigo[1]

SELECT COUNT(*)
INTO :li_Estado
FROM dbo.palletencab
WHERE clie_codigo = :li_Cliente
AND   plde_codigo <> :li_Planta
AND   paen_numero = :al_Pallet
AND   paen_estado = 1;

IF li_Estado > 0 THEN
	MessageBox("Cuidado","Este Pallet Está en Existencia en Otra Planta")
END IF

RETURN


end subroutine

public function boolean existepallet (string ls_columna);Integer		li_cliente, li_tiporec, li_ptadest, li_ptaorig, li_cantid, li_sinencabezado,&
            li_donde, li_usda
Long			ll_nropal, ll_fila, ll_filad, ll_pcopda
String		ls_embarque, ls_embalaje
Boolean		lb_retorno = False
DataStore	ds_encabezado, ds_detalle

ds_encabezado	 =	CREATE DataStore 
ds_detalle		 =	CREATE DataStore

li_cliente					= 	dw_2.Object.clie_codigo[1]
li_ptadest					=	dw_2.Object.plde_codigo[1]
li_tiporec					=	Integer(istr_mant.argumento[20])
li_ptaorig					=	Integer(istr_mant.argumento[21])
ls_embarque					=	istr_mant.argumento[44]
ll_nropal 					= 	Long(ls_columna)
istr_mant.argumento[2]	= 	String(ll_nropal)
ls_Embalaje					=	EmbalajeCliente(li_Cliente)

li_sinencabezado			=	PalletSinEncab(li_ptadest,li_Cliente,ll_nropal)

SELECT	Count(*)
	INTO	:li_cantid
	FROM	dbo.palletencab_trans
	WHERE	clie_codigo		= :li_cliente
	AND	paen_numero    = :ll_nropal ;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
	lb_retorno = True
ELSEIF li_cantid > 0 THEN
	CHOOSE CASE li_tiporec
		CASE 0,1,4,5
			
			   SELECT	Count(*)
				INTO	:li_usda
				FROM	dbo.palletencab_trans
				WHERE	clie_codigo		= :li_cliente
				AND	paen_numero    = :ll_nropal
				AND   paen_estado in (7,8);
			
////			IF li_sinencabezado > 0 THEN
//				MessageBox("Atención","Nro. de Pallet ya existe para este Cliente.  Ingrese otro.", &
//					Exclamation!, Ok!)
//				lb_retorno = True
//			
////			END IF	

            IF li_sinencabezado = 0 AND li_usda = 0 THEN
               
					
					super::existepallet(ls_columna)
					
					
            ELSE
	   			MessageBox("Atención","Nro. de Pallet ya existe para este Cliente.  Ingrese otro.", &
					Exclamation!, Ok!)
		   		lb_retorno = True
			
   			END IF	
					
			
		CASE 2
			SELECT	Count(*)
				INTO 	:li_cantid
				FROM	dbo.palletencab_trans
				WHERE	clie_codigo	=	:li_cliente
				AND	paen_numero	=	:ll_nropal
				AND	plde_codigo	=	:li_ptaorig
				AND   paen_estado not in (7,8);

				IF sqlca.SQLCode = -1 THEN
					lb_retorno = True
				ELSEIF li_cantid = 0 THEN
						SELECT	Count(*)
						INTO 	:li_donde
						FROM	dbo.palletencab_trans
						WHERE	clie_codigo	=	:li_cliente
						AND	paen_numero	=	:ll_nropal
						AND	plde_codigo	=	:li_ptadest ;
					   IF sqlca.SQLCode = -1 THEN
					      RETURN True
						ELSEIF li_donde > 0 THEN
								 MessageBox("Atención","Pallet ya existe en esta Planta.", &
								 Exclamation!, Ok!)
								 RETURN True
						END IF
					END IF			
			IF sqlca.SQLCode = -1 THEN
				F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
				lb_retorno = True
			ELSEIF li_cantid > 0 THEN
				ds_encabezado.DataObject	=	"dw_mant_palletencab"
				ds_detalle.DataObject		=	"dw_mues_palletfruta"
				
				ds_encabezado.GetChild("clie_codigo", dw_cliente)
				ds_encabezado.GetChild("plde_codigo", dw_planta)
				ds_encabezado.GetChild("espe_codigo", dw_especie)
				ds_encabezado.GetChild("etiq_codigo", dw_etiqueta)
				ds_encabezado.GetChild("tpem_codigo", dw_emba)
				
				dw_cliente.SetTransObject(sqlca)
				dw_planta.SetTransObject(sqlca)
				dw_especie.SetTransObject(sqlca)
				dw_etiqueta.SetTransObject(sqlca)
				dw_emba.SetTransObject(sqlca)
				
				dw_cliente.Retrieve(li_cliente)
				dw_planta.Retrieve(1)
				dw_especie.Retrieve()
				dw_etiqueta.Retrieve()
				dw_emba.Retrieve(li_cliente, ls_Embalaje)
								
				ds_encabezado.SetTransObject(sqlca)
				ds_detalle.SetTransObject(sqlca)

				ds_encabezado.Retrieve(li_cliente, ll_nropal, li_ptaorig)
				ds_detalle.Retrieve(li_cliente, ll_nropal, li_ptaorig)
				//
				dw_2.SetItem(1, "paen_tipopa", ds_encabezado.Object.paen_tipopa[1])
				dw_2.SetItem(1, "tpem_codigo", ds_encabezado.Object.tpem_codigo[1])
				dw_2.SetItem(1, "espe_codigo", ds_encabezado.Object.espe_codigo[1])
				dw_2.SetItem(1, "vari_codigo", ds_encabezado.Object.vari_codigo[1])
				dw_2.SetItem(1, "vari_nombre", ds_encabezado.Object.vari_nombre[1])
				dw_2.SetItem(1, "tiem_codigo", ds_encabezado.Object.tiem_codigo[1])
				dw_2.SetItem(1, "emba_codigo", ds_encabezado.Object.emba_codigo[1])
				dw_2.SetItem(1, "emba_nombre", ds_encabezado.Object.emba_nombre[1])
				dw_2.SetItem(1, "cate_codigo", ds_encabezado.Object.cate_codigo[1])
				dw_2.SetItem(1, "etiq_codigo", ds_encabezado.Object.etiq_codigo[1])
				dw_2.SetItem(1, "stat_codigo", ds_encabezado.Object.stat_codigo[1])
				dw_2.SetItem(1, "trat_codigo", ds_encabezado.Object.trat_codigo[1])
				dw_2.SetItem(1, "frio_codigo", ds_encabezado.Object.frio_codigo[1])
				dw_2.SetItem(1, "cond_codigo", ds_encabezado.Object.cond_codigo[1])
				dw_2.SetItem(1, "dest_codigo", ds_encabezado.Object.dest_codigo[1])
				dw_2.SetItem(1, "paen_fecemb", ds_encabezado.Object.paen_fecemb[1])
				dw_2.SetItem(1, "paen_cosecha", ds_encabezado.Object.paen_cosecha[1])
				dw_2.SetItem(1, "paen_altura", ds_encabezado.Object.paen_altura[1])
				dw_2.SetItem(1, "paen_ccajas", ds_encabezado.Object.paen_ccajas[1])
				dw_2.SetItem(1, "tmvp_codigo", ds_encabezado.Object.tmvp_codigo[1])
				dw_2.SetItem(1, "paen_fecini", ds_encabezado.Object.paen_fecini[1])
				dw_2.SetItem(1, "paen_horain", ds_encabezado.Object.paen_horain[1])
				dw_2.SetItem(1, "cama_codigo", ds_encabezado.Object.cama_codigo[1])
				dw_2.SetItem(1, "paen_calle", ds_encabezado.Object.paen_calle[1])

				dw_2.SetItem(1, "paen_base", ds_encabezado.Object.paen_base[1])
				dw_2.SetItem(1, "paen_posici", ds_encabezado.Object.paen_posici[1])
				dw_2.SetItem(1, "paen_estado", 1)
				dw_2.SetItem(1, "paen_inspec", ds_encabezado.Object.paen_inspec[1])
				dw_2.SetItem(1, "paen_concal", ds_encabezado.Object.paen_concal[1])
				dw_2.SetItem(1, "paen_pexpor", ds_encabezado.Object.paen_pexpor[1])
				dw_2.SetItem(1, "paen_pmixto", ds_encabezado.Object.paen_pmixto[1])
				dw_2.SetItem(1, "copa_codigo", ds_encabezado.Object.copa_codigo[1])
				//
				FOR ll_fila	=	1 TO ds_detalle.RowCount()
					ll_filad	=	dw_1.InsertRow(0)
					dw_1.SetItem(ll_filad, "clie_codigo", li_cliente)
					dw_1.SetItem(ll_filad, "paen_numero", ll_nropal)
					dw_1.SetItem(ll_filad, "espe_codigo", ds_detalle.Object.espe_codigo[ll_fila])
					dw_1.SetItem(ll_filad, "vari_codigo", ds_detalle.Object.vari_codigo[ll_fila])
					dw_1.SetItem(ll_filad, "emba_codigo", ds_detalle.Object.emba_codigo[ll_fila])
					dw_1.SetItem(ll_filad, "prod_codigo", ds_detalle.Object.prod_codigo[ll_fila])
					dw_1.SetItem(ll_filad, "productores_prod_nombre", ds_detalle.Object.productores_prod_nombre[ll_fila])
					dw_1.SetItem(ll_filad, "cond_codigo", ds_detalle.Object.cond_codigo[ll_fila])
					dw_1.SetItem(ll_filad, "etiq_codigo", ds_detalle.Object.etiq_codigo[ll_fila])
					dw_1.SetItem(ll_filad, "plde_codigo", li_ptadest)
					dw_1.SetItem(ll_filad, "pafr_calibr", ds_detalle.Object.pafr_calibr[ll_fila])
					dw_1.SetItem(ll_filad, "pafr_secuen", ds_detalle.Object.pafr_secuen[ll_fila])
					dw_1.SetItem(ll_filad, "pafr_ccajas", ds_detalle.Object.pafr_ccajas[ll_fila])
					dw_1.SetItem(ll_filad, "pafr_nrlote", ds_detalle.Object.pafr_nrlote[ll_fila])
					
					dw_1.SetItem(ll_Filad, "pafr_copack", ds_detalle.Object.pafr_copack[ll_Fila])
					dw_1.SetItem(ll_Filad, "pafr_fecemb", ds_detalle.Object.pafr_fecemb[ll_Fila])
					dw_1.SetItem(ll_Filad, "pafr_cuart1", ds_detalle.Object.pafr_cuart1[ll_Fila])
					dw_1.SetItem(ll_filad, "pafr_huert1", ds_detalle.Object.pafr_huert1[ll_Fila])
				NEXT
				istr_mant.argumento[3]	=	String(dw_2.Object.espe_codigo[1])
				istr_mant.argumento[4]	=	String(dw_2.Object.vari_codigo[1])
				istr_mant.argumento[5]	= 	dw_2.Object.vari_nombre[1]
				istr_mant.argumento[6]	= 	String(dw_2.Object.plde_codigo[1])
				istr_mant.argumento[7]	= 	dw_2.Object.emba_codigo[1]
				istr_mant.argumento[8]	= 	dw_2.Object.emba_nombre[1]
				istr_mant.argumento[9]	= 	String(dw_2.Object.etiq_codigo[1])
				istr_mant.argumento[10] = 	String(dw_2.Object.cond_codigo[1])
				istr_mant.argumento[12] = 	""
				cuentacajas()
				ExisteTipoEmbalaje(String(dw_2.Object.tpem_codigo[1]))
				ExistePlanta(dw_2.Object.plde_codigo[1])
				Existecondicion(dw_2.Object.cond_codigo[1])
				IF noesnuevo(li_ptadest,li_cliente, ll_nropal) THEN
					pb_eliminar.Enabled	= False
					pb_grabar.Enabled		= False
					UPDATE dbo.palletencab	SET
					   paen_estado = 1
						WHERE	clie_codigo	=	:li_cliente
						AND	paen_numero	=	:ll_nropal
						AND	plde_codigo	=	:li_ptadest ;
					commit;
				ELSE
					pb_eliminar.Enabled	= True
					pb_grabar.Enabled		= True
				END IF

				END IF
		CASE 3,7
			SELECT	Count(*)
				INTO 	:li_cantid
				FROM	dbo.palletencab_trans
				WHERE	clie_codigo	=	:li_cliente
				AND	paen_numero	=	:ll_nropal
				AND	plde_codigo	=	:li_ptadest ;
							
			IF sqlca.SQLCode = -1 THEN
				F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
				lb_retorno = True
			ELSEIF li_cantid = 0 THEN
				MessageBox("Atención","Nro. de Pallet No Existe en esta Planta.", &
					Exclamation!, Ok!)
				lb_retorno = True
			ELSE
				IF li_tiporec = 3 THEN
					SELECT	Count(*)
						INTO 	:li_cantid
						FROM	dbo.despafrigode as det, dbo.despafrigoen as enc
						WHERE	det.plde_codigo	=	:li_ptadest
						AND	det.clie_codigo	=	:li_cliente
						AND	det.paen_numero	=	:ll_nropal
						AND	enc.plde_codigo	=	det.plde_codigo
						AND	enc.defe_numero	=	det.defe_numero
						AND	enc.embq_codigo	=	:ls_embarque ;
				ELSE
					SELECT	Count(*)
						INTO 	:li_cantid
						FROM	dbo.palletencab_trans
						WHERE	clie_codigo	=	:li_cliente
						AND	paen_numero	=	:ll_nropal
						AND	plde_codigo	=	:li_ptadest
						AND   paen_estado =  8;

				END IF
				IF sqlca.SQLCode = -1 THEN
					F_errorbasedatos(sqlca,"Lectura tablas Despafrigode / Despafrigoen")
					lb_retorno = True
				ELSEIF IsNull(li_cantid) OR li_cantid = 0 THEN
					MessageBox("Atención","Pallet No Disponible.", &
						Exclamation!, Ok!)
					   lb_retorno = True
				ELSE
					This.TriggerEvent("ue_recuperadatos")
					istr_mant.argumento[3] = String(dw_2.Object.espe_codigo[1])
					istr_mant.argumento[4] = String(dw_2.Object.vari_codigo[1])
					istr_mant.argumento[5] = dw_2.Object.vari_nombre[1]
					istr_mant.argumento[6] = String(dw_2.Object.plde_codigo[1])
					istr_mant.argumento[7] = dw_2.Object.emba_codigo[1]
					istr_mant.argumento[8] = dw_2.Object.emba_nombre[1]
					istr_mant.argumento[9] = String(dw_2.Object.etiq_codigo[1])
					istr_mant.argumento[10] = String(dw_2.Object.cond_codigo[1])
					istr_mant.argumento[12] = ""
					ExisteTipoEmbalaje(String(dw_2.Object.tpem_codigo[1]))
					ExistePlanta(dw_2.Object.plde_codigo[1])
					Existecondicion(dw_2.Object.cond_codigo[1])
					IF li_tiporec = 3 THEN
						dw_2.SetItem(1, "paen_estado", 1)
					ELSE
						dw_2.SetItem(1, "paen_estado", 7)
					END IF
				END IF
			END IF

		CASE 6
			
			SELECT	Count(*)
				INTO 	:li_cantid
				FROM	dbo.palletencab_trans
				WHERE	clie_codigo	=	:li_cliente
				AND	paen_numero	=	:ll_nropal
				AND	plde_codigo	=	:li_ptadest 				
				AND   paen_estado not in (7,8);
							
			IF sqlca.SQLCode = -1 THEN
				F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
				lb_retorno = True
			ELSEIF li_cantid = 0 THEN
				MessageBox("Atención","Pallet NO ha pertenecido a esta Planta.", &
					Exclamation!, Ok!)
			//	lb_retorno = True
			ELSE
							
				SELECT	Count(*)
					INTO 	:li_cantid
					FROM	dbo.palletencab
					WHERE	clie_codigo	=	:li_cliente
					AND	paen_numero	=	:ll_nropal
					AND	plde_codigo	=	:li_ptadest
				   AND   paen_estado =  1;
					
					IF li_cantid > 0 THEN
						MessageBox("Atención","Pallet con estado de Existencia en esta Planta.", &
						Exclamation!, Ok!)
						//lb_retorno = True
				   ELSE					
		
						SELECT	Count(*)
							INTO 	:li_cantid
							FROM	dbo.despafrigode as det, dbo.despafrigoen as enc
							WHERE	det.plde_codigo	=	:li_ptadest
							AND	det.clie_codigo	=	:li_cliente
							AND	det.paen_numero	=	:ll_nropal
							AND	enc.plde_codigo	=	det.plde_codigo
							AND	enc.defe_numero	=	det.defe_numero;
						
						IF sqlca.SQLCode = -1 THEN
							F_errorbasedatos(sqlca,"Lectura tablas Despafrigode / Despafrigoen")
							lb_retorno = True
						ELSEIF li_cantid = 0 THEN
							MessageBox("Atención","No Existe Despacho para Reingresar Nro.de Pallet.", &
								Exclamation!, Ok!)
							lb_retorno = True
						ELSE
						//	istr_mant.argumento[6] = String(li_ptaorig)
							
							
							This.TriggerEvent("ue_recuperadatos")
							istr_mant.argumento[3] = String(dw_2.Object.espe_codigo[1])
							istr_mant.argumento[4] = String(dw_2.Object.vari_codigo[1])
							istr_mant.argumento[5] = dw_2.Object.vari_nombre[1]
							istr_mant.argumento[6] = String(dw_2.Object.plde_codigo[1])
							istr_mant.argumento[7] = dw_2.Object.emba_codigo[1]
							istr_mant.argumento[8] = dw_2.Object.emba_nombre[1]
							istr_mant.argumento[9] = String(dw_2.Object.etiq_codigo[1])
							istr_mant.argumento[10] = String(dw_2.Object.cond_codigo[1])
							istr_mant.argumento[12] = ""
							ExisteTipoEmbalaje(String(dw_2.Object.tpem_codigo[1]))
							ExistePlanta(dw_2.Object.plde_codigo[1])
							Existecondicion(dw_2.Object.cond_codigo[1])
							dw_2.SetItem(1, "paen_estado", 1)
						//	dw_2.Object.plde_codigo[1] = li_ptadest
//							dw_2.SetItem(1, "paen_inspec", 0)
//							dw_2.SetItem(1, "dest_codigo", 999)

							istr_mant.Solo_Consulta = False
						END IF
					END IF
				END IF					
					
	END CHOOSE
ELSEIF li_tiporec = 7 THEN
		MessageBox("Atención","No Existe Pallet en Sitio USDA.  Ingrese otro.", &
		Exclamation!, Ok!)
  		lb_retorno = True
ELSE
	SELECT Count(*)
		INTO	:ll_pcopda
		FROM	dbo.spro_cajasprodpallet
		WHERE	clie_codigo	= 	:li_cliente
		AND	capr_numpal = 	:ll_nropal
		AND	plde_codigo	=	:li_ptadest;
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla spro_cajasprodpallet")
		lb_retorno = True
	ELSEIF ll_pcopda > 0 THEN	
			MessageBox("Atención","El Numero de Pallet Ingresado es un Ingreso Caja a Caja. Ingrese Otro.", Exclamation!, Ok!)
			lb_retorno = True	
	END IF
END IF

DESTROY ds_encabezado
DESTROY ds_detalle

RETURN lb_retorno
end function

public function boolean grabadocrel ();String	ls_embala
Integer	li_cliente, li_cancaj, li_planta
Long		ll_pallet, ll_docrel

li_cliente	= Integer(istr_mant.argumento[1])
li_planta	= Integer(istr_mant.argumento[6])
ll_pallet   = Long(istr_mant.argumento[2])

SELECT	DISTINCT pafr_docrel
	INTO	:ll_docrel
	FROM	dbo.spro_palletfruta
	WHERE	clie_codigo	=	:li_Cliente
	AND	paen_numero	=	:ll_pallet
	AND	plde_codigo	=	:li_planta;
		
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_palletfruta")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	//MessageBox("Atención","Tipo de Embalaje no Existe para este Cliente.~rIngrese Otro.", Exclamation!, Ok!)
	RETURN False
ELSE
	
	UPDATE dbo.palletfruta_trans SET
		pafr_docrel = :ll_docrel
		WHERE clie_codigo = :li_cliente
		AND	plde_codigo = :li_planta
		AND	paen_numero = :ll_pallet;
		
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla palletfruta")
		RETURN False
	END IF
		
	UPDATE dbo.palletfrutahisto SET
		pafr_docrel = :ll_docrel
		WHERE clie_codigo = :li_cliente
		AND	plde_codigo = :li_planta
		AND	paen_numero = :ll_pallet;
		
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla palletfrutahisto")
		RETURN False
	END IF
	
	Commit;
	
	RETURN True
END IF

RETURN False
end function

public function boolean noesnuevo (integer planta, integer cliente, long numero);Long	registros
  
SELECT	Count(*)  
	INTO	:registros
  	FROM	dbo.palletencab_trans
  	WHERE plde_codigo	=	:planta
	AND  	clie_codigo	=	:cliente
	AND 	paen_numero	=	:numero ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla palletencab")
	RETURN False
ELSEIF registros > 0 THEN
	RETURN True
END IF

RETURN False
end function

public function boolean noexistetarja (integer planta, integer folio, integer cliente, long pallet);Long	registros
  
SELECT	Count(*)  
	INTO	:registros
  	FROM	dbo.recfruproced_trans
  	WHERE plde_codigo	=	:planta
  	AND	rfpe_numero	=	:folio
	AND  	clie_codigo	=	:cliente
	AND 	paen_numero	=	:pallet ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Recfruproced")
	RETURN FALSE
ELSEIF registros > 0 THEN
	MessageBox("Atención","Nro de Pallet ya ingresado para el Nro. de Folio", Exclamation!, Ok!)
	RETURN FALSE
END IF

RETURN TRUE
end function

public function boolean noexistetarjare (integer planta, integer folio, integer cliente, long pallet);Long	registros
  
  SELECT Count(paen_numero)  
    INTO :registros
    FROM dbo.Repalletdeta
   WHERE plde_codigo = :planta AND  
         repe_numero = :folio AND  
         clie_codigo = :cliente AND  
         paen_numero = :pallet;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Repalletdeta")
	RETURN FALSE
ELSEIF registros > 0 THEN
	MessageBox("Atención","Nro de Pallet ya ingresado para el Nro. de Folio", Exclamation!, Ok!)
	RETURN FALSE
END IF

RETURN TRUE
end function

public function integer palletsinencab (integer planta, integer cliente, long numero);Long	registros=0,registros1=0, registros2=0
  
SELECT	Count(*)  
	INTO	:registros1
  	FROM	dbo.recfruproced_trans as re, dbo.palletencab_trans as pe
  	WHERE re.plde_codigo	=	:planta
	AND  	re.clie_codigo	=	:cliente
	AND 	re.paen_numero	=	:numero
	AND   pe.paen_estado =  1
	AND   re.clie_codigo =  pe.clie_codigo
	AND   re.plde_codigo =  pe.plde_codigo
	AND   re.paen_numero =  pe.paen_numero;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Recfruproced")
	registros1=1
END IF

SELECT	Count(*)  
	INTO	:registros2
  	FROM	dbo.repalletdeta
  	WHERE plde_codigo	=	:planta
	AND  	clie_codigo	=	:cliente
	AND 	paen_numero	=	:numero ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Repalletdeta")
	registros2=1
END IF

registros = registros1 + registros2

RETURN registros
end function

protected function boolean wf_actualiza_db (boolean borrando);if not dw_2.uf_check_required(0) then return false
if not dw_1.uf_validate(0) then return false
if borrando then
	if dw_1.Update() = 1 and dw_2.Update() = 1 then
		commit;
		if sqlca.sqlcode <> 0 then
				f_errorbasedatos(sqlca,"Lista la tabla")
			return false
		else
			return true
		end if 
	else
		rollback;
		if sqlca.sqlcode <> 0 then f_errorbasedatos(sqlca,"Lista la tabla")
		return false
	end if
else
	if dw_2.update() = 1 and dw_1.Update() = 1  then 
		commit;
		if sqlca.sqlcode <> 0 then
			f_errorbasedatos(sqlca,"Lista la tabla")
			return false
		else
			return true
		end if 
	else
		rollback;
		if sqlca.sqlcode <> 0 then f_errorbasedatos(sqlca,"Lista la tabla")
		return false
	end if
end if

return true
end function

public function boolean existepallet_definitivo (string ls_columna);Integer	li_cliente, li_planta, li_Movto
Long		ll_nropal, ll_pcopda, ll_cantid, ll_PFruta
Boolean	lb_retorno = False

li_cliente					=	Integer(dw_2.GetItemNumber(1, "clie_codigo"))
li_planta					=	Integer(dw_2.GetItemNumber(1, "plde_codigo"))
ll_nropal 					=	Long(ls_columna)

SELECT	Count(*)
	INTO	:ll_cantid
	FROM	dbo.palletencab
	WHERE	clie_codigo	= 	:li_cliente
	AND	paen_numero = 	:ll_nropal
	AND	plde_codigo	=	:li_planta;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
	lb_retorno = True
ELSEIF ll_cantid > 0  and Integer(istr_mant.argumento[50]) <> 6  THEN
	MessageBox("Atención","El Numero de Pallet Ingresado es un Ingreso Definitivo. Ingrese Otro.", Exclamation!, Ok!)
	istr_mant.UsuarioSoloConsulta	=	True
	lb_retorno = True		
END IF	
	   
RETURN lb_retorno

end function

public function boolean existe_pallet_devolucion (string ls_columna);Integer	li_cliente, li_planta, li_Movto
Long		ll_nropal, ll_pcopda, ll_cantid, ll_PFruta
Boolean	lb_retorno = False

li_cliente					=	Integer(dw_2.GetItemNumber(1, "clie_codigo"))
li_planta					=	Integer(dw_2.GetItemNumber(1, "plde_codigo"))
ll_nropal 					=	Long(ls_columna)

SELECT	Count(*)
	INTO	:ll_cantid
	FROM	dbo.palletencab
	WHERE	clie_codigo	= 	:li_cliente
	AND	paen_numero = 	:ll_nropal
	AND	plde_codigo	=	:li_planta
	AND	paen_estado = 	2;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
	lb_retorno = True
ELSEIF ll_cantid > 0 THEN
	//MessageBox("Atención","El Numero de Pallet Ingresado es un Ingreso Definitivo. Ingrese Otro.", Exclamation!, Ok!)
	//istr_mant.UsuarioSoloConsulta	=	True
	lb_retorno = False
ELSE
	MessageBox("Atención","El Numero de Pallet No existe o NO ha Sido Despachado de Esta Planta. Ingrese Otro.", Exclamation!, Ok!)
	istr_mant.UsuarioSoloConsulta	=	True
	lb_retorno = True
END IF	
	   
RETURN lb_retorno

end function

public function boolean existecapturapallet (integer ai_cliente, integer ai_planta, long al_pallet);Integer		li_Existe, li_Estado, li_pedesp
Long			ll_cajas
String		ls_tpemcodigo

SELECT Count(*)
INTO  :li_Existe
FROM  dbo.palletencab
WHERE clie_codigo = :ai_Cliente
AND   plde_codigo = :ai_Planta
AND   paen_numero = :al_Pallet;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Palletencab")
	RETURN False
ELSEIF li_Existe = 0 THEN
	MessageBox("Error","Pallet No Existe", &
					Information!, Ok!)					
	RETURN False
END IF


//SELECT paen_estado,paen_ccajas,paen_pedesp
//INTO  :li_Estado,:ll_cajas,:li_pedesp
//FROM  dbo.spro_palletencab
//WHERE clie_codigo = :ai_Cliente
//AND   plde_codigo = :ai_Planta
//AND   paen_numero = :al_Pallet;
//
//IF sqlca.SQLCode = -1 THEN
//	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Spro_Palletencab")
//	RETURN False
//ELSEIF Not IsNull(li_Estado) AND li_Estado = 2 THEN
//	MessageBox("Error","Pallet Ya Fue Recepcionado", &
//					Information!, Ok!)					
//	RETURN False
//ELSEIF Isnull(li_pedesp) OR li_pedesp = 0 THEN
//	
//	   SELECT max(tpem_codigo)
//		INTO  :ls_tpemcodigo
//		FROM  dbo.spro_palletfruta
//		WHERE clie_codigo = :ai_Cliente
//		AND   plde_codigo = :ai_Planta
//		AND   paen_numero = :al_Pallet
//		AND	pafr_secuen > 0;
//	
//		IF Long(ls_tpemcodigo) <> ll_cajas THEN
//				MessageBox("Error","Pallet No es Pedido Especial y No Contiene Total de Cajas", &
//								Information!, Ok!)					
//				RETURN False			
//		END IF
//END IF

//IF ll_cajas > 0 THEN
//	pb_grabar.Enabled = True
//	il_pallet = al_Pallet
//ELSE
//	MessageBox("Error","Pallet No Existe en Packing", &
//				Information!, Ok!)	
//	RETURN False				
//
//END IF	


pb_grabar.Enabled = True

RETURN True

end function

public function boolean duplicado (integer ai_cliente, long al_numero);Long		ll_Fila

ll_Fila	=	dw_2.Find("clie_codigo = " + String(ai_Cliente) + &
							 " AND plde_codigo = " + istr_mant.Argumento[1] + &
							 " AND paen_numero = " + String(al_Numero) , 1, &
							 dw_2.RowCount())
	
IF ll_Fila > 0  THEN
	MessageBox("Error","Pallet ya fue incluido en Detalle de Recepción", &
					Information!, Ok!)					
	RETURN True
END IF
	
RETURN False
end function

event open;/*
	Argumentos	:	[1]	=	Código de Planta
						[2]	=	Número de Folio Despacho
						[3]	=	Código de Cliente
*/
This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant	=	Message.PowerObjectParm

dw_1.GetChild("tpem_codigo", dw_emba)
dw_emba.SetTransObject(sqlca)
dw_emba.Retrieve(Integer(istr_mant.argumento[3]), 'Z')

This.title = 'DEVOLUCION DE PUERTO'

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)


istr_mant.dw.ShareData(dw_1)

//iuo_Pallet						=	Create uo_palletencab
//iuo_Variedad					=	Create uo_variedades
//iuo_analizapallet				=	Create uo_analizapallet  
//

end event

on w_maed_palletencab_recepcion_devolucion.create
int iCurrent
call super::create
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
end on

on w_maed_palletencab_recepcion_devolucion.destroy
call super::destroy
destroy(this.dw_3)
end on

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
//
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_fila, ll_inspec, ll_prod,ll_cajas,ll_fil1, ll_productor, ll_Numero, ll_fila2
Integer  li_clien,li_especie,li_vari,li_planta, li_codpak , &
			li_cliente, li_variedad, li_etique, li_cond, li_Secuen
String   ls_emba,ls_calibr, ls_embala, ls_calibre
Date     ld_fecemb

IF Message.DoubleParm = -1 THEN //RETURN
ELSE
	IF UpperBound(istr_mant2.Argumento) < 21 THEN
		istr_mant2.Argumento[21] = ''
	END IF
	
	IF UpperBound(istr_mant3.Argumento) < 2 THEN
		istr_mant3.Argumento[2] = ''
	END IF
	
	IF istr_mant2.argumento[20] <> '3' AND istr_mant2.argumento[20] <> '6' AND & 
		istr_mant2.argumento[20] <> '2'THEN
		IF dw_1.rowcount() > 0 THEN
			FOR ll_fil1=1 to dw_1.rowcount()
				 //IF istr_mant2.argumento[31] = '1' then
					 dw_1.Object.pafr_copack[ll_fil1] = Integer(istr_mant2.argumento[21])
				 //ELSE
				 //	 dw_1.Object.pafr_copack[ll_fil1] = -1
				 //END IF
			NEXT
		END IF
	END IF
	
	iuo_variedadrotula.existe(dw_2.Object.espe_codigo[1],dw_2.Object.vari_codigo[1], True, sqlca) 
	
	dw_2.Object.paen_varrot[1] = iuo_variedadrotula.Varirotula
	
	FOR ll_fila2 = 1 TO dw_1.RowCount()
		dw_1.Object.pafr_varrot[ll_fila2]	=	iuo_variedadrotula.Varirotula
	NEXT
	
	
	IF istr_mant2.Argumento[5] <> 'dw_mues_repalletdeta' THEN
		
		// Para guardar la fecha de Ingreso cuando es Packing
		IF istr_mant2.argumento[13] = '2' AND istr_mant2.argumento[39] <> '' THEN
			IF dw_1.rowcount() > 0 THEN
				FOR ll_fil1=1 to dw_1.rowcount()			
					 dw_1.Object.pafr_fecing[ll_fil1] = Date(istr_mant2.argumento[39])
				NEXT
			END IF
		END IF
		
		// Para guardar la fecha de Ingreso cuando traspaso Inperplanta
		IF istr_mant2.argumento[13] = '1' AND istr_mant2.argumento[39] <> '' THEN
			IF dw_1.rowcount() > 0 THEN
				FOR ll_fil1=1 to dw_1.rowcount()		
					 IF Isnull(dw_1.Object.pafr_fecing[ll_fil1]) OR dw_1.Object.pafr_fecing[ll_fil1] = Date('1900-01-01') THEN
						 dw_1.Object.pafr_fecing[ll_fil1] = Date(istr_mant2.argumento[39])
					END IF
				NEXT
			END IF
		END IF
	END IF
	
	IF Isnull(dw_2.Object.paen_fecemb[1]) THEN
		Messagebox("Atención","No ha Ingresado Fecha de Embalaje",exclamation!)
		dw_2.setcolumn("paen_fecemb")
		dw_2.setfocus()
		RETURN
	END IF
	
	IF dw_2.Object.paen_tipopa[1] = 1 THEN
		IF Isnull(dw_2.Object.tpem_codigo[1]) THEN
			Messagebox("Atención","No ha Ingresado Tipo Pallet",exclamation!)
			dw_2.setcolumn("tpem_codigo")
			dw_2.setfocus()
			Message.DoubleParm = -1
			RETURN
		END IF
	END IF	
	
	IF not existe_recprefriopda(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_2.Object.paen_numero[1] ) THEN
		dw_2.SetItem(1, "cama_codigo", 0)
		dw_2.SetItem(1, "paen_calle", 1)
		dw_2.SetItem(1, "paen_base", 1)
		dw_2.SetItem(1, "paen_posici", 1)
		dw_2.SetItem(1, "tmvp_codigo", 0)
	ELSE
		dw_2.SetItem(1, "cama_codigo", i_vec_ubicacion[1])
		dw_2.SetItem(1, "paen_calle", i_vec_ubicacion[2])
		dw_2.SetItem(1, "paen_base", i_vec_ubicacion[3])
		dw_2.SetItem(1, "paen_posici", i_vec_ubicacion[4])
		dw_2.SetItem(1, "tmvp_codigo", i_vec_ubicacion[5])
	END IF	
	
	IF gb_Repalletizado THEN
		ll_fila	=	dw_3.InsertRow(0)
		dw_3.Object.plde_codigo[ll_fila]		=	dw_2.Object.plde_codigo[1]
		dw_3.Object.clie_codigo[ll_fila]		=	Integer(istr_mant.Argumento[1])
		dw_3.Object.paen_numero[ll_fila]		=	dw_2.Object.paen_numero[1]
		dw_3.Object.repe_numero[ll_fila]		=	Long(istr_mant2.Argumento[2])
		dw_3.Object.plde_codigo[ll_fila]		=	Integer(istr_mant2.Argumento[1])
		dw_3.Object.repd_tipood[ll_fila]		=	2
		dw_3.Object.vari_nombre[ll_fila]		=	dw_2.Object.vari_nombre[1]
		dw_3.Object.emba_codigo[ll_fila]		=	dw_2.Object.emba_codigo[1]
		dw_3.Object.cate_codigo[ll_fila]		=	dw_2.Object.cate_codigo[1]
		dw_3.Object.stat_codigo[ll_fila]		=	dw_2.Object.stat_codigo[1]
		dw_3.Object.espe_codigo[ll_fila]		=	dw_2.Object.espe_codigo[1]			
		dw_3.Object.vari_codigo[ll_fila]		=	dw_2.Object.vari_codigo[1]
		dw_3.Object.etiq_codigo[ll_fila]		=	dw_2.Object.etiq_codigo[1]
		dw_3.Object.trat_codigo[ll_fila]		=	dw_2.Object.trat_codigo[1]
		dw_3.Object.frio_codigo[ll_fila]		=	dw_2.Object.frio_codigo[1]
		dw_3.Object.cond_codigo[ll_fila]		=	dw_2.Object.cond_codigo[1]
		dw_3.Object.paen_fecemb[ll_fila]		=	dw_2.Object.paen_fecemb[1]
		dw_3.Object.paen_cosecha[ll_fila]	=	dw_2.Object.paen_cosecha[1]
		dw_3.Object.paen_altura[ll_fila]		=	dw_2.Object.paen_altura[1]
		dw_3.Object.copa_codigo[ll_fila]		=	dw_2.Object.copa_codigo[1]
		dw_3.Object.tiem_codigo[ll_fila]		=	dw_2.Object.tiem_codigo[1]
		dw_3.Object.paen_tipopa[ll_fila]		=	dw_2.Object.paen_tipopa[1]
		dw_3.Object.tpem_codigo[ll_fila]		=	dw_2.Object.tpem_codigo[1]
		dw_3.Object.paen_ccajas[ll_fila]		=	dw_2.Object.paen_ccajas[1]	
		
		IF istr_mant2.Argumento[5] = 'dw_mues_repalletdeta' THEN
			dw_3.Object.pafr_secuen[ll_fila]		=	ll_fila	
		END IF
			
		IF dw_2.Object.paen_inspec[1] = 1 THEN
			ll_inspec	=	Long(String(Long(istr_mant3.Argumento[1]),'00000'))
				
		END IF
	
	ELSE
		IF NoExisteTarja(Integer(istr_mant.Argumento[1]), Integer(istr_mant2.Argumento[1]), &
							Long(istr_mant2.Argumento[2]), dw_2.Object.paen_numero[1]) THEN	
			//	Actualiza Planta y Estado Existencia en caso de ser Recepción Interplanta
			dw_2.SetItem(1, "plde_codigo", Integer(istr_mant2.argumento[1]))
			dw_2.SetItem(1, "paen_estado", 1)
			
			//	Actualiza Usuario
			dw_2.SetItem(1,"paen_pcopda",1)
			dw_2.SetItem(1,"paen_usuari",gstr_us.Nombre)
			dw_2.SetItem(1,"paen_estaci",gstr_us.Computador)
	
			//	Cambio en Condición
			FOR ll_Fila = 1 TO dw_1.RowCount()
				dw_1.SetItem(ll_Fila, "plde_codigo", dw_2.Object.plde_codigo[1])
				dw_1.SetItem(ll_Fila, "cond_codigo", dw_2.Object.cond_codigo[1])			
			NEXT
	
			ll_fila	=	dw_3.InsertRow(0)
			IF IsNull(istr_mant2.Argumento[6]) OR istr_mant2.Argumento[6] = "" THEN
				dw_3.Object.plde_codigo[ll_fila]		=	dw_2.Object.plde_codigo[1]
				dw_3.Object.rfpe_numero[ll_fila]		=	Long(istr_mant2.Argumento[2])
				dw_3.Object.clie_codigo[ll_fila]		=	dw_2.Object.clie_codigo[1]
				dw_3.Object.paen_numero[ll_fila]		=	dw_2.Object.paen_numero[1]
				dw_3.Object.rfpe_pcopda[ll_Fila]		=	1
				dw_3.Object.vari_nombre[ll_fila]		=	dw_2.Object.vari_nombre[1]
				dw_3.Object.emba_codigo[ll_fila]		=	dw_2.Object.emba_codigo[1]
				dw_3.Object.cate_codigo[ll_fila]		=	dw_2.Object.cate_codigo[1]
				dw_3.Object.stat_codigo[ll_fila]		=	dw_2.Object.stat_codigo[1]
				dw_3.Object.espe_codigo[ll_fila]		=	dw_2.Object.espe_codigo[1]			
				dw_3.Object.vari_codigo[ll_fila]		=	dw_2.Object.vari_codigo[1]
				dw_3.Object.etiq_codigo[ll_fila]		=	dw_2.Object.etiq_codigo[1]
				dw_3.Object.trat_codigo[ll_fila]		=	dw_2.Object.trat_codigo[1]
				dw_3.Object.frio_codigo[ll_fila]		=	dw_2.Object.frio_codigo[1]
				dw_3.Object.cond_codigo[ll_fila]		=	dw_2.Object.cond_codigo[1]
				dw_3.Object.paen_fecemb[ll_fila]		=	dw_2.Object.paen_fecemb[1]
				dw_3.Object.paen_cosecha[ll_fila]	=	dw_2.Object.paen_cosecha[1]
				dw_3.Object.paen_altura[ll_fila]		=	dw_2.Object.paen_altura[1]
				dw_3.Object.copa_codigo[ll_fila]		=	dw_2.Object.copa_codigo[1]
				dw_3.Object.tiem_codigo[ll_fila]		=	dw_2.Object.tiem_codigo[1]
				dw_3.Object.paen_tipopa[ll_fila]		=	dw_2.Object.paen_tipopa[1]
				dw_3.Object.tpem_codigo[ll_fila]		=	dw_2.Object.tpem_codigo[1]
				dw_3.Object.paen_ccajas[ll_fila]		=	dw_2.Object.paen_ccajas[1]			
			END IF
			
			IF dw_2.Object.paen_inspec[1] = 1 THEN
				ll_inspec	=	Long(String(Integer(istr_mant2.Argumento[21]),'00') + String(Long(istr_mant3.Argumento[1]),'0000'))
	
			END IF
			
		END IF 
	END IF
END IF
end event

event ue_guardar;call super::ue_guardar;//grabadocrel()
end event

event ue_listo;call super::ue_listo;pb_ins_det.Enabled	= False

dw_2.SetColumn("paen_fecemb")
end event

event ue_modifica_detalle;IF istr_mant2.argumento[7] <> '1' THEN
	Cuentacajas()
	
	IF dw_1.RowCount() > 0 THEN
		istr_mant.argumento[1]	=	String(dw_2.getitemNumber(1,"clie_codigo"))
		istr_mant.argumento[2]	=	String(dw_2.getitemNumber(1,"paen_numero"))
		istr_mant.argumento[3]	=	String(dw_2.getitemNumber(1,"espe_codigo"))
//		istr_mant.argumento[4]	=	String(dw_2.getitemNumber(1,"vari_codigo"))
		
//		istr_mant.argumento[4]	=	String(dw_1.Object.vari_codigo[il_fila]) 
//	   istr_mant.argumento[7]	=	dw_1.Object.emba_codigo[il_fila] 
		
		istr_mant.agrega 			= False
		istr_mant.solo_consulta	= False
		
		IF istr_mant2.Argumento[40] = '1' OR istr_mant.argumento[50] = '5' THEN	
			OpenWithParm(w_mant_deta_palletfruta_interfecemb_trans, istr_mant)
		ELSE	
			OpenWithParm(iw_mantencion, istr_mant)
		END IF	
	END IF
END IF
end event

event ue_nuevo;//Boolean	lb_TerminaPallet, lb_terminaPucho
Integer	li_Pallets, li_Puchos, li_clien, li_planta, li_especie, li_vari, li_codpak, li_etiq, li_concod
Long		ll_modif1, ll_modif2, ll_prod, ll_cajas, ll_fila, ll_secuen, ll_paen, ll_dataw3
String   ls_emba, ls_calibr
Date     ld_fecemb

ib_ok	= True

CHOOSE CASE  wf_modifica()
	CASE -1
		ib_ok = False
	CASE 0
		ll_modif1	=	dw_1.GetNextModified(0, Primary!)
		ll_modif2	=	dw_2.GetNextModified(0, Primary!)
	
//		IF ib_ModEncab OR dw_1.GetNextModified(0, Primary!) > 0 THEN
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

IF ib_ok = False THEN RETURN

dw_1.Reset()

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

dw_2.Object.paen_numero.Protect	=	0
dw_2.Object.paen_numero.BackGround.Color = RGB(255,255,255)
dw_2.SetRedraw(True)

dw_2.SetFocus()
dw_2.SetColumn("paen_numero")
dw_2.Setitem(1,"clie_codigo", Integer(istr_mant.argumento[1]))
dw_2.SetItem(1,"plde_codigo", Integer(istr_mant2.argumento[1]))
dw_2.SetItem(1,"espe_codigo", gi_CodEspecie)

IF istr_mant.argumento[17] = '1'and istr_mant.argumento[24] <> '1' THEN
	dw_2.SetItem(1,"paen_fecemb",Date(istr_mant.argumento[28]))
	dw_2.SetItem(1,"paen_cosecha",Date(istr_mant.argumento[28]))
END IF	

istr_mant.argumento[3]	=	String(gi_CodEspecie)
istr_mant.argumento[6]	= 	istr_mant2.argumento[1]
istr_mant.argumento[9]	= 	"1"
istr_mant.argumento[10]	=	"0"

IF ii_CantPallets=0 THEN
	dw_2.SetItem(1, "paen_tipopa", 2)
	dw_2.Object.paen_tipopa.Protect	=	1
	lb_TerminaPallet						=	True
END IF

IF dw_3.RowCount() > 0 THEN
	li_Pallets	=	dw_3.GetItemNumber(1, "pallets")
	li_Puchos	=	dw_3.GetItemNumber(1, "puchos")

	IF ii_CantPallets > 0 AND dw_3.GetItemNumber(1, "pallets") = ii_CantPallets THEN
		IF Not lb_MensPallet THEN
			MessageBox("Atención", "Se ha Completado la Cantidad de Pallets~r" + &
							"indicados en la Recepción")
			
			lb_MensPallet	=	True
		END IF
		
		dw_2.SetItem(1, "paen_tipopa", 2)
		
		dw_2.Object.paen_tipopa.Protect	=	1
		lb_TerminaPallet						=	True
	END IF
	
	IF ii_CantPuchos > 0 AND dw_3.GetItemNumber(1, "puchos") = ii_CantPuchos THEN
		IF Not lb_MensPucho THEN
			MessageBox("Atención", "Se ha Completado la Cantidad de Puchos~r" + &
							"indicados en la Recepción")
			
			lb_MensPucho	=	True
		END IF
		
		dw_2.SetItem(1, "paen_tipopa", 1)
		
		dw_2.Object.paen_tipopa.Protect	=	1
		lb_TerminaPucho						=	True
	END IF
	
	IF lb_TerminaPallet AND lb_TerminaPucho THEN
		pb_salir.TriggerEvent(Clicked!)
	ELSE
		istr_mant.argumento[3]	=	String(dw_2.Object.espe_codigo[1])
		istr_mant.argumento[6]	=	istr_mant2.argumento[1]
		istr_mant.argumento[9]	=	"1"
		istr_mant.argumento[10]	=	"0"
		
		IF Long(istr_mant2.argumento[6]) <> 0 THEN
			dw_2.SetFocus()
			dw_2.SetColumn("paen_numero")
			//dw_2.Setitem(1,"paen_numero", Long(istr_mant2.argumento[6]))	
			//dw_2.AcceptText()
			//dw_2.PostEvent(itemchanged!)
		END IF
	   ll_dataw3	=	dw_3.RowCount()
		IF ll_dataw3 > 0 THEN
			
			 dw_2.Setitem(1,"plde_codigo", dw_3.Object.plde_codigo[ll_dataw3])
			 istr_mant.argumento[6]		=	String(dw_3.Object.plde_codigo[ll_dataw3])
			 dw_2.Setitem(1,"emba_codigo", dw_3.Object.emba_codigo[ll_dataw3])
			 istr_mant.argumento[7]		=	dw_3.Object.emba_codigo[ll_dataw3]
			 dw_2.Setitem(1,"stat_codigo", dw_3.Object.stat_codigo[ll_dataw3])
			 dw_2.Setitem(1,"espe_codigo", dw_3.Object.espe_codigo[ll_dataw3])
			 istr_mant.argumento[3]		=	String(dw_3.Object.espe_codigo[ll_dataw3])
			 dw_2.Setitem(1,"vari_codigo", dw_3.Object.vari_codigo[ll_dataw3])
			 istr_mant.argumento[4]		=	String(dw_3.Object.vari_codigo[ll_dataw3])
			 dw_2.Setitem(1,"etiq_codigo", dw_3.Object.etiq_codigo[ll_dataw3])
			 istr_mant.argumento[9]		=	String(dw_3.Object.etiq_codigo[ll_dataw3])
			 dw_2.Setitem(1,"trat_codigo", dw_3.Object.trat_codigo[ll_dataw3])
			 dw_2.Setitem(1,"frio_codigo", dw_3.Object.frio_codigo[ll_dataw3])
			 dw_2.Setitem(1,"cond_codigo", dw_3.Object.cond_codigo[ll_dataw3])
			 istr_mant.argumento[10]	=	String(dw_3.Object.cond_codigo[ll_dataw3])
			 dw_2.Setitem(1,"paen_fecemb", dw_3.Object.paen_fecemb[ll_dataw3])
			 istr_mant.argumento[40]	=	String(dw_3.Object.paen_fecemb[ll_dataw3])		 
			 dw_2.Setitem(1,"paen_cosecha", dw_3.Object.paen_cosecha[ll_dataw3])
			 dw_2.Setitem(1,"paen_altura", dw_3.Object.paen_altura[ll_dataw3])
			 dw_2.Setitem(1,"copa_codigo", dw_3.Object.copa_codigo[ll_dataw3])
			 dw_2.Setitem(1,"cate_codigo", dw_3.Object.cate_codigo[ll_dataw3])
			 IF lb_MensPallet = False THEN
				dw_2.Setitem(1,"tiem_codigo", dw_3.Object.tiem_codigo[ll_dataw3])			
				dw_2.Setitem(1,"paen_tipopa", dw_3.Object.paen_tipopa[ll_dataw3])			
				dw_2.Setitem(1,"tpem_codigo", dw_3.Object.tpem_codigo[ll_dataw3])			
				dw_2.Setitem(1,"paen_ccajas", dw_3.Object.paen_ccajas[ll_dataw3])			
			 END IF
		END IF
	END IF
END IF
end event

event ue_nuevo_detalle;date fecha
fecha=(dw_2.Object.paen_fecemb[1])

IF istr_mant2.Argumento[5] = 'dw_mues_repalletdeta' THEN
	istr_mant.borra	= False
	istr_mant.agrega	= True
	cuentacajas()
	istr_mant.argumento[28]=String(dw_2.object.paen_fecemb[1])
	
	istr_mant.argumento[45]=	istr_mant2.argumento[32]		// Fecha de Ingreso
	istr_mant.argumento[46]=	istr_mant2.argumento[33]		// Fecha de Embalaje
	istr_mant.argumento[47]=	istr_mant2.argumento[8]		// Productores
	// w_mant_deta_palletfruta
	
	OpenWithParm(w_mant_deta_palletfruta_repa, istr_mant)
	
	IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
	
	IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
		pb_eliminar.Enabled	= TRUE
		pb_grabar.Enabled		= TRUE
	END IF
	
	dw_1.SetRow(il_fila)
	dw_1.SelectRow(il_fila,True)
ELSEIF istr_mant2.Argumento[40] = '1' OR istr_mant.argumento[50] = '5' THEN	
	istr_mant.borra	= False
	istr_mant.agrega	= True
	cuentacajas()
	istr_mant.argumento[28]=String(dw_2.object.paen_fecemb[1])
	
	OpenWithParm(w_mant_deta_palletfruta_interfecemb_trans, istr_mant)
	
	IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
	
	IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
		pb_eliminar.Enabled	= TRUE
		pb_grabar.Enabled		= TRUE
	END IF
	
	dw_1.SetRow(il_fila)
	dw_1.SelectRow(il_fila,True)
ELSE
	call super::ue_nuevo_detalle
	
END IF
end event

event ue_recuperadatos;//
end event

event ue_seleccion;//
end event

type dw_1 from w_maed_palletencab_trans`dw_1 within w_maed_palletencab_recepcion_devolucion
integer x = 142
integer y = 324
integer height = 1124
string dataobject = "dw_mues_recfruproced_caja"
boolean righttoleft = false
end type

type dw_2 from w_maed_palletencab_trans`dw_2 within w_maed_palletencab_recepcion_devolucion
integer x = 471
integer y = 52
integer width = 1472
integer height = 180
string dataobject = "dw_mant_palletencab_devolucion"
end type

event dw_2::itemchanged;Integer	li_Cliente, li_Nulo, li_Estiba, li_CanPal, li_Especie, li_variedad
Long		ll_NroPallet, ll_Nulo
String	ls_Nulo, ls_Columna

SetNull(ls_Nulo)

ls_Columna	=	dwo.Name
					
CHOOSE CASE ls_Columna
	CASE "paen_numero"
		
		li_Cliente						=	Integer(istr_mant.argumento[3])
		ll_NroPallet					= 	Long(data)
		
		IF len(data) = 18 THEN
			ll_NroPallet = Long(mid(data,12,6))
			dw_2.Object.paen_numero[il_fila] = ll_NroPallet
		END IF
				
		IF ( Duplicado(Integer(istr_mant.argumento[3]),ll_NroPallet) OR &
		   Not ExisteCapturaPallet(Integer(istr_mant.argumento[3]),&
				Integer(istr_mant.argumento[1]), ll_NroPallet) ) THEN
			This.SetItem(il_fila, ls_Columna, Long(ls_Nulo))
			RETURN 1
		END IF

END CHOOSE
end event

type pb_nuevo from w_maed_palletencab_trans`pb_nuevo within w_maed_palletencab_recepcion_devolucion
boolean visible = false
end type

type pb_eliminar from w_maed_palletencab_trans`pb_eliminar within w_maed_palletencab_recepcion_devolucion
boolean visible = false
end type

type pb_grabar from w_maed_palletencab_trans`pb_grabar within w_maed_palletencab_recepcion_devolucion
integer x = 3296
end type

event pb_grabar::clicked;//
Integer 	li_Fila, li_filanew, li_cliente, li_planta, li_tipoen, li_Tipocamion, li_puerto, li_transportista, li_TipoFruta
Long		ll_numero, ll_numerores
String		ls_nombre, ls_computador, ls_patente, ls_chofer, ls_embarque, ls_Observacion

If Not IsNull(dw_2.Object.paen_numero[1]) Then 	
	If dw_2.rowcount() > 0 Then
		li_cliente 		= Integer(istr_mant.argumento[3])
		li_planta 			= Integer(istr_mant.argumento[1])
		ll_numero 		= Long(istr_mant.Argumento[37])
		ll_numerores	= Long(istr_mant.Argumento[30])
				
		li_Tipocamion 		=	Integer(istr_mant.Argumento[40])
		ls_embarque 		=	istr_mant.Argumento[41]
		li_puerto 			=  Integer(istr_mant.Argumento[42])
		li_transportista 		=  Integer(istr_mant.Argumento[43])
		ls_patente 			=  istr_mant.Argumento[44]	
		ls_chofer		 		=  istr_mant.Argumento[45]	
		il_pallet				=	dw_2.Object.paen_numero[1]
		li_TipoFruta			=	Integer(istr_mant.Argumento[46])
		ls_Observacion		=	istr_mant.Argumento[47]
		li_tipoen				=	3
		
		ls_nombre		= gstr_us.nombre
		ls_computador	= gstr_us.computador
		
		DECLARE Traspaso_recepcion_caja PROCEDURE FOR dbo.Fproc_Recepcion_devolucion			
			@Cliente			=	:li_Cliente,			      
			@Planta			=	:li_Planta,
			@pallet			=	:il_pallet,
			@numero  		=  :ll_numero,
			@numerores	=	:ll_numerores,
			@tipoen			=  :li_tipoen,
			@usuario		=	:ls_nombre,
			@computador 	= 	:ls_computador,
			@embarque		=	:ls_embarque,
			@puerto 			=	:li_puerto,
			@tipocamion	 =	:li_Tipocamion,
			@trancodigo	 =	:li_transportista,
			@patente 		=	:ls_patente,
			@chofer			=	:ls_chofer,
			@TipoFruta 		=	:li_TipoFruta,
			@Observacion	=	:ls_Observacion;		
		EXECUTE Traspaso_recepcion_caja;
		
		If sqlca.SQLCode = -1 Then
			F_ErrorBaseDatos(sqlca, "Lectura Tabla PalletFruta")
		End If	
		
		Commit;
		 
		w_maed_recfruprocee_particular_devolucion.TriggerEvent("ue_traedatos")
		
//		dw_6.Retrieve(li_Cliente,il_pallet,li_Planta)
		
		dw_2.Reset()
		dw_2.InsertRow(0)
		dw_2.SetColumn("paen_numero")
		dw_2.SetFocus()
	End If
End If
end event

type pb_imprimir from w_maed_palletencab_trans`pb_imprimir within w_maed_palletencab_recepcion_devolucion
boolean visible = false
end type

type pb_salir from w_maed_palletencab_trans`pb_salir within w_maed_palletencab_recepcion_devolucion
end type

event pb_salir::clicked;grabadocrel()
Close(Parent)

//Integer	li_respue
//
//IF istr_mant2.agrega = False AND istr_mant2.borra = False THEN
//	CloseWithReturn(Parent, istr_mant2)
//ELSE
//	IF dw_3.RowCount() < ii_CantPallets + ii_CantPuchos THEN
//		li_respue	=	MessageBox("ADVERTENCIA","No se ha completado la cantidad de Pallets~r" + &
//							"indicados en la Recepción.~r~rDesea retornar a ventana de recepción.", &
//							Question!, YesNo!)
//			IF li_respue = 1 THEN
//				CloseWithReturn(Parent, istr_mant2)
//			ELSE
//				pb_nuevo.SetFocus()
//			END IF
//	ELSE
//		CloseWithReturn(Parent, istr_mant2)
//	END IF
//END IF
//	
//
end event

type pb_ins_det from w_maed_palletencab_trans`pb_ins_det within w_maed_palletencab_recepcion_devolucion
boolean visible = false
integer y = 1488
end type

type pb_eli_det from w_maed_palletencab_trans`pb_eli_det within w_maed_palletencab_recepcion_devolucion
boolean visible = false
end type

type pb_buscar from w_maed_palletencab_trans`pb_buscar within w_maed_palletencab_recepcion_devolucion
boolean visible = false
end type

type dw_histoencab from w_maed_palletencab_trans`dw_histoencab within w_maed_palletencab_recepcion_devolucion
integer x = 1637
integer y = 2144
end type

type dw_histofruta from w_maed_palletencab_trans`dw_histofruta within w_maed_palletencab_recepcion_devolucion
integer x = 0
integer y = 2020
end type

type dw_3 from datawindow within w_maed_palletencab_recepcion_devolucion
integer x = 3264
integer y = 1892
integer width = 686
integer height = 400
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_palletfruta_caja"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

