$PBExportHeader$w_maed_movtofrutagranel_proceso.srw
$PBExportComments$Proceso de Fruta Granel Interplanta y Devolución a Productor
forward
global type w_maed_movtofrutagranel_proceso from w_mant_encab_deta_csd
end type
type tab_1 from tab within w_maed_movtofrutagranel_proceso
end type
type tp_1 from userobject within tab_1
end type
type dw_detalle from uo_dw within tp_1
end type
type tp_1 from userobject within tab_1
dw_detalle dw_detalle
end type
type tp_2 from userobject within tab_1
end type
type dw_envases from uo_dw within tp_2
end type
type tp_2 from userobject within tab_1
dw_envases dw_envases
end type
type tab_1 from tab within w_maed_movtofrutagranel_proceso
tp_1 tp_1
tp_2 tp_2
end type
type dw_3 from datawindow within w_maed_movtofrutagranel_proceso
end type
type dw_10 from datawindow within w_maed_movtofrutagranel_proceso
end type
type dw_lotpro from datawindow within w_maed_movtofrutagranel_proceso
end type
end forward

global type w_maed_movtofrutagranel_proceso from w_mant_encab_deta_csd
integer width = 4229
integer height = 1960
string title = "TRASPASO DE FRUTA GRANEL A PROCESO"
string menuname = ""
windowstate windowstate = maximized!
event ue_imprimir ( )
tab_1 tab_1
dw_3 dw_3
dw_10 dw_10
dw_lotpro dw_lotpro
end type
global w_maed_movtofrutagranel_proceso w_maed_movtofrutagranel_proceso

type variables
w_selecciona_lotes_existencia	iw_mantencion_1
w_mant_deta_movtoenvadeta	iw_mantencion_2

datastore							ids_detalle

DataWindowChild   				idwc_PltaDest, & 
										idwc_Camara, idwc_TipoEnvase, idwc_PltaLote, idwc_Especie, &
										idwc_Envases, idwc_Linea

DataWindowChild   				idwc_Camara1, idwc_TipoEnvase1, idwc_Especie1, &
										idwc_Envases1, idwc_Packing, idwc_Camara2

DataWindowChild					idwc_planta, idwc_productor, idwc_periodo, idwc_tipo, ids_tipo, &
                     						ids_camara, ids_envase,idwc_plantadw1,idwc_pladesdw1

DataWindow						dw_4, dw_5

str_variedad							istr_variedad
str_categoria						istr_categoria

uo_plantadesp						iuo_Packing
uo_envases							iuo_envases
uo_lotesfrutagranel				iuo_Lotes
uo_CamarasFrigo					iuo_Camara
uo_tipomovtofruta					iuo_TipoMovtoFruta
uo_tipomovtofruta					iuo_TipoMovtoEnva
uo_control_historico_proceso	iuo_historico

Long     								il_NumFruta=0, il_NumEnva=0
Boolean								ib_Modifica, ib_AutoCommit, ib_borra=True
Integer								il_eleccion=1, il_tipoen, il_envacodigo, ii_Orpr_TipOrd
Long     								il_secuencia
String									is_columna, is_sermed, is_lote
end variables

forward prototypes
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine reingresaexistencia ()
public function boolean existemovtoproceso (integer ai_planta, integer ai_tipomovto, integer ai_tipodocto, integer al_proceso)
public function boolean existelote (string as_lote)
public subroutine insertaexistencia (integer ai_camac, string as_lote, integer ai_tipoe, integer ai_envac, long al_fila)
public function boolean existedocproceso (integer ai_planta, long al_numero)
public function long existebultos (integer ai_camac)
public subroutine ingresaregistros ()
public function boolean insertadetallegranel ()
public function boolean lotesdestarados (string as_lote)
public subroutine revisa_registros (boolean ab_tipo_revisa)
public subroutine habilitaencab (boolean habilita)
public function boolean existedespacho (integer ai_planta, integer ai_tipomovto, long al_numero)
public function boolean duplicado (string as_columna, string as_valor)
public subroutine habilitaingreso (string as_columna)
public subroutine buscaenvase ()
public function boolean noexistecliente (integer cliente)
public function boolean historial ()
public function boolean procesopacking ()
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long			fila, ll_fila, ll_cant, ll_lote, ll_totlot, ll_FilaTot
Integer  	li_tipdoc, li_docrel, li_planta, li_especie
str_info		lstr_info

lstr_info.titulo	= "TRASPASO DE FRUTA GRANEL A PROCESO"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

IF dw_2.Object.mfge_estmov[1] > 1 THEN
	vinf.dw_1.DataObject = "dw_info_termino_de_proceso"
	vinf.dw_1.SetTransObject(sqlca)
	
	li_tipdoc = dw_2.Object.defg_tipdoc[1]
	li_docrel = dw_2.Object.defg_docrel[1]
	
	fila = vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]), li_tipdoc,li_docrel,Integer(istr_mant.Argumento[16]))

ELSE	
	vinf.dw_1.DataObject = "dw_info_movtofrutagranel_proceso"
	vinf.dw_1.SetTransObject(sqlca)
	
	fila = vinf.dw_1.Retrieve(integer(istr_mant.argumento[1]), integer(istr_mant.argumento[2]),&
									  integer(istr_mant.argumento[3]),Integer(istr_mant.Argumento[16]))
END IF

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila =0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Object.DataWindow.Zoom = 95
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

protected function boolean wf_actualiza_db (boolean borrando);Boolean			lb_Retorno
long           ll_fila, ll_planta, ll_numero
Integer			li_cliente, li_tipord


IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

IF Borrando THEN
	IF dw_5.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			IF dw_4.Update(True, False) = 1 THEN
				IF dw_2.Update(True, False) = 1 THEN
					IF Historial() THEN
						
						li_cliente	=	dw_2.GetItemNumber(1, "clie_codigo", Delete!, False)
						ll_planta	=	dw_2.GetItemNumber(1, "plde_codigo", Delete!, False)
						li_tipord	=	dw_2.GetItemNumber(1, "orpr_tipord", Delete!, False)
						ll_numero	=	dw_2.GetItemNumber(1, "defg_docrel", Delete!, False)

						UPDATE dbo.spro_ordenproceso
							SET orpr_estado = 1
						 WHERE clie_codigo =:li_cliente
						 	AND plde_codigo =:ll_planta
							AND orpr_tipord =:li_tipord
							AND orpr_numero =:ll_numero;
							
						IF sqlca.SQLCode <> 0 THEN
							F_ErrorBaseDatos(sqlca, This.Title)
					
							RollBack;
						ELSE
							Commit;
				
							IF sqlca.SQLCode <> 0 THEN
								F_ErrorBaseDatos(sqlca, This.Title)
							ELSE
								lb_Retorno	=	True
					
								dw_1.ResetUpdate()
								dw_2.ResetUpdate()
								dw_4.ResetUpdate()
								dw_5.ResetUpdate()
							END IF
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
		IF dw_4.Update(True, False) = 1 THEN
			IF dw_1.Update(True, False) = 1 THEN
				IF dw_5.Update(True, False) = 1 THEN
					IF Historial() THEN
						li_cliente	=	dw_2.Object.clie_codigo[1]
						ll_planta	=	dw_2.Object.plde_codigo[1]
						li_tipord	=	dw_2.Object.orpr_tipord[1]
						ll_numero	=	dw_2.Object.defg_docrel[1]

						UPDATE dbo.spro_ordenproceso
							SET orpr_estado = 2
						 WHERE clie_codigo =:li_cliente
						 	AND plde_codigo =:ll_planta
							AND orpr_tipord =:li_tipord
							AND orpr_numero =:ll_numero;
							
						IF sqlca.SQLCode = -1 THEN
							F_ErrorBaseDatos(sqlca, "Lectura de Tabla Ordenes de Proceso")
							lb_Retorno	=	False
						ELSE
							Commit;
				
							IF sqlca.SQLCode <> 0 THEN
								F_ErrorBaseDatos(sqlca, This.Title)
							ELSE
								lb_Retorno	=	True
					
								dw_1.ResetUpdate()
								dw_2.ResetUpdate()
								dw_4.ResetUpdate()
								dw_5.ResetUpdate()
							END IF
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

sqlca.AutoCommit	=	ib_AutoCommit

FOR ll_fila=1 to dw_4.rowcount()
  dw_4.SetItemStatus(ll_fila,0,Primary!, dataModified!)
NEXT



RETURN lb_Retorno
end function

public subroutine reingresaexistencia ();Integer li_row, li_planta, li_cama, li_plt,li_esp,li_lot, li_tipoenva, li_envase
String  ls_lote

li_planta=gstr_paramplanta.codigoplanta
//dw_3.reset()
//
//For li_row=1 To dw_4.rowcount()
//	
//  li_cama		=	dw_4.Object.cama_codigo[li_row] 	
//  ls_lote		=	dw_4.Object.lotes[li_row]
//  li_plt 		= 	Integer(Mid(ls_lote,1,4))
//  li_esp 		= 	Integer(Mid(ls_lote,5,2))
//  li_lot 		= 	Integer(Mid(ls_lote,7,4))
//  li_tipoenva	=	dw_4.Object.enva_tipoen[li_row]
//  li_envase		=	dw_4.Object.enva_codigo[li_row]
//  
//  IF isnull(li_cama)=False or isnull(ls_lote)=False or ls_lote<>"" or &
//     isnull(li_tipoenva)=False or isnull(li_envase)=False THEN
//     insertaexistencia(li_planta,li_cama,li_plt,li_esp,li_lot,li_tipoenva,li_envase,ls_lote,li_row)
//	END IF
//NEXT	
end subroutine

public function boolean existemovtoproceso (integer ai_planta, integer ai_tipomovto, integer ai_tipodocto, integer al_proceso);Long		ll_Numero
Boolean	lb_Retorno = True
Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.Argumento[16])

SELECT	mfge_numero 
  INTO	:ll_Numero
  FROM	dbo.spro_movtofrutagranenca
 WHERE	plde_codigo	=	:ai_Planta
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	defg_tipdoc	=	:ii_orpr_tipord
	AND	defg_docrel	=	:al_Proceso
	AND   clie_codigo =  :li_Cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Despachos Fruta Granel")
	lb_Retorno	=	False

ELSEIF sqlca.SQLCode = 0 THEN
	istr_mant.Argumento[3]	=	String(ll_Numero)
	This.TriggerEvent("ue_recuperadatos")

ELSE
	lb_Retorno	=	False

END IF

RETURN lb_Retorno
end function

public function boolean existelote (string as_lote);Integer li_canbul, li_plt, li_esp, li_lot, li_Cliente
String  ls_nombre
Boolean lb_Retorno=True

li_plt 		= 	Integer(Mid(as_lote,1,4))
li_esp 		= 	Integer(Mid(as_lote,5,2))
li_lot 		= 	Integer(Mid(as_lote,7,4))
li_Cliente	=	Integer(istr_mant.argumento[16])

SELECT lote_codigo
  INTO :li_canbul
  FROM dbo.spro_lotesfrutagranel
 WHERE lote_pltcod = :li_plt
	AND lote_espcod = :li_esp
	AND lote_codigo = :li_lot;
//	AND clie_codigo = :li_Cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Lotes Fruta Granel")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Número de Lote No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF	

IF lb_Retorno THEN
 dw_4.setitem(il_fila,'lote_pltcod',li_plt)	
 dw_4.setitem(il_fila,'lote_espcod',li_plt)
 dw_4.setitem(il_fila,'lote_codigo',li_plt)
 dw_4.setitem(il_fila,'clie_codigo',li_cliente) 

END IF	
RETURN lb_retorno
end function

public subroutine insertaexistencia (integer ai_camac, string as_lote, integer ai_tipoe, integer ai_envac, long al_fila);Integer li_banda, li_pos, li_pis, li_canbul, li_categoria, li_planta, &
        li_lotep, li_lotee, li_lotec, li_Cliente
Long    ll_fila
String  ls_nombre

li_planta		=	dw_2.Object.plde_codigo[1]
li_lotep 		= 	Integer(Mid(as_lote,1,4))
li_lotee 		= 	Integer(Mid(as_lote,5,2))
li_lotec 		= 	Integer(Mid(as_lote,7,4))
li_Cliente		=	Integer(istr_mant.argumento[16])

SELECT caex_nroban, caex_nropos, caex_nropis, caex_canbul
  INTO :li_banda, :li_pos, :li_pis, :li_canbul
  FROM dbo.spro_camaraexistefg
 WHERE plde_codigo 	= 	:li_planta
	AND cama_codigo 	=	:ai_camac
	AND lote_pltcod 	= 	:li_lotep
	AND lote_espcod 	= 	:li_lotee
	AND lote_codigo 	= 	:li_lotec
	AND enva_tipoen 	= 	:ai_tipoe
	AND enva_codigo 	= 	:ai_envac;

dw_3.SetItem(al_Fila, "caex_canbul", li_canbul)
dw_3.SetItem(al_Fila, "caex_nroban", li_banda)
dw_3.SetItem(al_Fila, "caex_nropos", li_pos)
dw_3.SetItem(al_Fila, "caex_nropis", li_pis)
dw_3.SetItem(al_Fila, "clie_codigo", li_Cliente)

SELECT cate_codigo
  INTO :li_categoria
  FROM dbo.spro_lotesfrutagranel
 WHERE lote_pltcod 	= 	:li_lotep
	AND lote_espcod 	= 	:li_lotee
	AND lote_codigo 	= 	:li_lotec;

dw_3.SetItem(al_Fila, "cate_codigo", li_categoria)



end subroutine

public function boolean existedocproceso (integer ai_planta, long al_numero);Long		ll_Numero,ll_Productor
Integer	li_Especie, li_Variedad, li_Cantidad, li_Vigencia, &
         li_periodo, li_linea, li_turno, li_Cliente
String   ls_Nombre, ls_frio
Boolean	lb_Retorno = True

li_Cliente	=	Integer(istr_mant.Argumento[16])

SELECT	op.orpr_numero, op.prod_codigo, op.espe_codigo, 
			op.vari_codigo, op.orpr_canbul, op.orpr_estado, 
			op.frio_tipofr, op.pefr_codigo, op.orpr_nrotur, 
			op.line_codigo, va.vari_nombre
  INTO	:ll_Numero,     :ll_Productor,  :li_Especie, 
			:li_Variedad,   :li_Cantidad,   :li_Vigencia, 
			:ls_frio, 		 :li_periodo,    :li_turno, 
			:li_linea,  	 :ls_Nombre
	FROM	dbo.spro_ordenproceso op, 
			dbo.variedades va
	WHERE	op.plde_codigo	=	:ai_Planta
	  And	op.orpr_tipord	=	:ii_orpr_tipord
	  And op.orpr_numero	=	:al_Numero 
	  And	va.espe_codigo	=	op.espe_codigo
	  And	va.vari_codigo	=	op.vari_codigo
	  And op.clie_codigo =  :li_Cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Ordenes de Proceso")
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
	IF li_Vigencia > 2 AND li_Vigencia <> 4 THEN
		MessageBox("Atención","Número de Orden de Proceso no se encuentra vigente. Ingrese Otro.")
		lb_Retorno	=	False	
   ELSE
		IF Not ExisteMovtoProceso(ai_Planta,Integer(istr_mant.argumento[2]),4,al_Numero) THEN
			istr_mant.Argumento[5] 		=	String(ll_Numero)
			istr_mant.Argumento[6]  	= 	String(ll_Productor)
			istr_mant.Argumento[7]  	= 	String(1)
			istr_mant.Argumento[8]  	= 	String(li_Especie)
			istr_mant.Argumento[9]  	= 	String(li_Variedad)
			istr_mant.Argumento[10] 	= 	ls_Nombre
			istr_mant.Argumento[11] 	= 	String(li_Cantidad)
			istr_mant.Argumento[12] 	=  ls_frio
			istr_mant.Argumento[13] 	=  String(li_periodo)
			istr_mant.Argumento[14] 	=	String(li_turno)
			
			dw_2.Object.prod_codigo[1]	=	Long(istr_mant.argumento[6])
			dw_2.Object.espe_codigo[1]	=	Integer(istr_mant.argumento[8])
			dw_2.Object.vari_codigo[1]		=	Integer(istr_mant.argumento[9])
			dw_2.Object.vari_nombre[1]	=	istr_mant.argumento[10]
			dw_2.Object.frio_tipofr[1]		=	istr_mant.argumento[12]
			dw_2.Object.pefr_codigo[1]		=	Integer(istr_mant.argumento[13])
			dw_2.Object.orpr_nrotur[1]		=	Integer(istr_mant.argumento[14])
			dw_2.Object.line_codigo[1]		=	li_Linea
			dw_2.Object.clie_codigo[1]		=	Integer(istr_mant.argumento[16])
			
		END IF
	END IF 
ELSE
	MessageBox("Atención","Número de Orden de Proceso No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function long existebultos (integer ai_camac);Integer li_canbul, li_plt, li_esp, li_lot, li_Cliente
Long    ll_fila,  lb_Retorno
String  ls_nombre

li_plt 		=	Integer(Mid(is_lote,1,4))
li_esp 		= 	Integer(Mid(is_lote,5,2))
li_lot 		= 	Integer(Mid(is_lote,7,4))
li_Cliente	=	Integer(istr_mant.argumento[16])

SELECT caex_canbul
  INTO :li_canbul
  FROM dbo.spro_camaraexistefg
 WHERE plde_codigo = :li_plt
	AND cama_codigo =	:ai_camac
	AND lote_pltcod = :li_plt
	AND lote_espcod = :li_esp
	AND lote_codigo = :li_lot
	AND enva_tipoen = :il_tipoen
	AND enva_codigo = :il_envacodigo;
//	AND clie_codigo = :li_Cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Camara Existe Fruta Granel")
	
	lb_Retorno	=	0
ELSEIF sqlca.SQLCode<>100 THEN
		IF li_canbul=0 THEN
			SELECT lotd_totbul
           INTO :li_canbul
           FROM dbo.spro_lotesfrutagrandeta
			 WHERE lote_pltcod = :li_plt
				AND lote_espcod = :li_esp
				AND lote_codigo = :li_lot
				AND enva_tipoen = :il_tipoen
				AND enva_codigo = :il_envacodigo;
//				AND clie_codigo = :li_Cliente;
          
	  		IF sqlca.SQLCode = -1 THEN
				F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Lote Fruta Granel")
				lb_Retorno	=	0
			ELSEIF sqlca.sqlcode<>100 THEN
				 lb_retorno=li_canbul
			END IF
		ELSE	
			lb_retorno=li_canbul
		END IF	
END IF	
RETURN lb_retorno
end function

public subroutine ingresaregistros ();
Integer il_row

dw_4.reset()

FOR il_row=1 to dw_3.Rowcount()
	
	dw_4.object.mfgd_secuen[il_row]		=	il_secuencia + il_row - 1
	dw_4.Object.lotes[il_row]				=	dw_3.Object.numero_lote[il_row]
	dw_4.Object.variedad[il_row]			=	dw_2.Object.variedad[1]
	dw_4.Object.enva_tipoen[il_row]		=	dw_3.Object.enva_tipoen[il_row]
	dw_4.Object.enva_codigo[il_row]		=	dw_3.Object.enva_codigo[il_row]
	dw_4.Object.enva_nombre[il_row]  	=	dw_3.Object.enva_nombre[il_row]
	dw_4.Object.cama_codigo[il_row]		=	dw_3.Object.cama_codigo[il_row]
	dw_4.Object.mfgd_bulent[il_row]		=	dw_3.Object.caex_canbul[il_row]
	dw_4.Object.bultosdesp[il_row]		=	dw_3.Object.caex_canbul[il_row]
	dw_4.Object.plde_codigo[il_row]		=	dw_3.Object.plde_codigo[il_row]
	dw_4.Object.lote_pltcod[il_row]		=	dw_3.Object.lote_pltcod[il_row]
	dw_4.Object.lote_espcod[il_row]		=	dw_3.Object.lote_espcod[il_row]
	dw_4.Object.lote_codigo[il_row]   	=	dw_3.Object.lote_codigo[il_row]
	dw_4.Object.clie_codigo[il_row]		=	Integer(istr_mant.argumento[16])

NEXT	


	
	
end subroutine

public function boolean insertadetallegranel ();Long			ll_Fila, ll_FilaNea, ll_Fila_e
Integer		li_Camara, li_PltCod, li_EspCod, li_Lote, li_TipoEnvase, li_CodEnvase, &
				li_CanBul, li_busca
String      ls_lote
Dec{3}		ld_KilNet
Boolean		lb_Retorno = True

datastore	lds_orden

lds_orden				=	Create DataStore
lds_orden.dataObject	= 	"dw_selecciona_deta_ordenes"
lds_orden.SetTransObject(Sqlca)

ll_fila_e	=	lds_orden.Retrieve(Integer(istr_Mant.Argumento[1]), long(istr_Mant.Argumento[5]),&
											 Integer(istr_Mant.Argumento[16]),ii_Orpr_TipOrd)

IF ll_fila_e = -1 THEN
	F_ErrorBaseDatos(SQLCA,"Lectura de Ordenes de Proceso")
	lb_Retorno	=	False				
END IF

FOR ll_Fila = 1 TO lds_orden.RowCount()

	li_busca			=	dw_4.Find("lote_pltcod = " + String(lds_orden.Object.lote_pltcod[ll_Fila]) + " AND " +&
										 "lote_espcod = " + String(lds_orden.Object.lote_espcod[ll_Fila]) + " AND " +&
										 "lote_codigo = " + String(lds_orden.Object.lote_codigo[ll_Fila]) + " AND " +&
										 "enva_tipoen = " + String(lds_orden.Object.enva_tipoen[ll_Fila]) + " AND " +&
										 "enva_codigo = " + String(lds_orden.Object.enva_codigo[ll_Fila]) ,&
										 1, lds_orden.RowCount())
	IF li_busca = 0 THEN									 
		il_Fila				=	dw_4.InsertRow(0)
		li_Camara		=	lds_orden.Object.cama_codigo[ll_Fila]
		li_PltCod			=	lds_orden.Object.lote_pltcod[ll_Fila]
		li_EspCod		=	lds_orden.Object.lote_espcod[ll_Fila]
		li_Lote			=	lds_orden.Object.lote_codigo[ll_Fila]
		li_TipoEnvase	=	lds_orden.Object.enva_tipoen[ll_Fila]
		li_CodEnvase	=	lds_orden.Object.enva_codigo[ll_Fila]
		li_CanBul			=	lds_orden.Object.orpd_canbul[ll_Fila]
		
		SELECT lotd_kilpro
		INTO :ld_KilNet
		FROM dbo.spro_lotesfrutagrandeta
		WHERE lote_pltcod =: li_PltCod
		AND	lote_espcod =: li_EspCod
		AND 	lote_codigo =: li_Lote
		AND 	enva_tipoen =: li_TipoEnvase
		AND 	enva_codigo =: li_CodEnvase;
		
		ls_lote			=  String(li_pltcod,'0000') + String(li_espcod,'00') + String(li_lote,'0000')
		
		dw_4.SetItem(il_Fila, "mfgd_secuen", il_fila)
		dw_4.SetItem(il_Fila, "plde_codigo", Integer(istr_mant.argumento[1]))
		dw_4.SetItem(il_Fila, "tpmv_codigo", Integer(istr_mant.argumento[2]))
		dw_4.SetItem(il_Fila, "lotes", ls_lote)
		dw_4.SetItem(il_Fila, "plde_coorde", Integer(istr_mant.argumento[15]))
		dw_4.SetItem(il_fila, "variedad",  istr_mant.argumento[10])
		dw_4.SetItem(il_Fila, "cama_codigo", li_Camara)
		dw_4.SetItem(il_Fila, "lote_pltcod", li_PltCod)
		dw_4.SetItem(il_Fila, "lote_espcod", li_EspCod)
		dw_4.SetItem(il_Fila, "lote_codigo", li_Lote)
		dw_4.SetItem(il_Fila, "enva_tipoen", li_TipoEnvase)
		dw_4.SetItem(il_Fila, "enva_codigo", li_CodEnvase)
		dw_4.SetItem(il_Fila, "clie_codigo", Integer(istr_mant.argumento[16]))
		
		IF iuo_envases.Existe(li_tipoenvase,li_codenvase, True, Sqlca) THEN
			dw_4.Setitem(il_fila,'enva_nombre',iuo_envases.nombre)
		END IF
		
		IF lds_orden.Object.caex_canbul[ll_Fila]<li_CanBul THEN
			li_Canbul		=	lds_orden.Object.caex_canbul[ll_Fila]
			ld_KilNet		=	li_Canbul * lds_orden.Object.lotd_kilpro[ll_Fila]
		END IF
		
		dw_4.SetItem(il_Fila, "mfgd_bulent", li_CanBul)
		dw_4.SetItem(il_Fila, "bultosdesp", li_Canbul)
		dw_4.SetItem(il_Fila, "mfgd_kgnent", ld_KilNet)
	
		dw_3.InsertRow(0)
		dw_3.SetItem(il_Fila, "plde_codigo", Integer(istr_mant.argumento[1]))
		dw_3.SetItem(il_Fila, "numero_lote", ls_lote)
		dw_3.SetItem(il_Fila, "cama_codigo", li_Camara)
		dw_3.SetItem(il_Fila, "lote_pltcod", li_PltCod)
		dw_3.SetItem(il_Fila, "lote_espcod", li_EspCod)
		dw_3.SetItem(il_Fila, "lote_codigo", li_Lote)
		dw_3.SetItem(il_Fila, "enva_tipoen", li_TipoEnvase)
		dw_3.SetItem(il_Fila, "enva_codigo", li_CodEnvase)
		dw_3.SetItem(il_Fila, "enva_nombre", lds_orden.Object.enva_nombre[ll_Fila])
		dw_3.SetItem(il_Fila, "cate_codigo", lds_orden.Object.cate_codigo[ll_Fila])
		dw_3.SetItem(il_Fila, "caex_nroban", lds_orden.Object.caex_nroban[ll_Fila])
		dw_3.SetItem(il_Fila, "caex_nropos", lds_orden.Object.caex_nropos[ll_Fila])
		dw_3.SetItem(il_Fila, "caex_nropis", lds_orden.Object.caex_nropis[ll_Fila])
		dw_3.SetItem(il_Fila, "caex_canbul", lds_orden.Object.caex_canbul[ll_Fila])
		dw_3.SetItem(il_Fila, "clie_codigo", Integer(istr_mant.argumento[16]))	
		
	END IF
	dw_4.ScrollToRow(il_fila)
	dw_4.SetRow(il_fila)

NEXT

RETURN lb_Retorno


end function

public function boolean lotesdestarados (string as_lote);Integer li_codigo, li_plt, li_esp, li_lot, li_Cliente
String  ls_nombre
Boolean lb_Retorno=True

li_plt 		= 	Integer(Mid(as_lote,1,4))
li_esp 		= 	Integer(Mid(as_lote,5,2))
li_lot 		= 	Integer(Mid(as_lote,7,4))
li_Cliente	=	Integer(istr_mant.argumento[16])

SELECT lote_codigo
  INTO :li_codigo
  FROM dbo.spro_lotesfrutagranel
 WHERE lote_pltcod = :li_plt
	AND lote_espcod = :li_esp
	AND lote_codigo = :li_lot
	AND lote_totnet > 0;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Lotes Fruta Granel")
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 100 THEN
	lb_Retorno	=	False
END IF	
RETURN lb_retorno

end function

public subroutine revisa_registros (boolean ab_tipo_revisa);Long     ll_fila, ll_row
Integer  li_envase, li_tipoenva, li_camara, li_pltcod, li_espcod,li_lotcod, li_Bultos, li_Cliente
Dec{3}	ld_KilNet
String   ls_Nrolote

li_Cliente	=	Integer(istr_mant.argumento[16])

IF ab_Tipo_Revisa THEN
	//Revisa los registros desde Existencia hacia Detalle, para Ingreso	
	FOR  ll_fila=1 TO dw_3.Rowcount()
 	
		ls_nrolote	= dw_3.Object.Numero_lote[ll_fila]
		li_tipoenva = dw_3.Object.enva_tipoen[ll_fila]
		li_envase	= dw_3.Object.enva_codigo[ll_fila]
		li_camara	= dw_3.Object.cama_codigo[ll_fila]

		ll_row	=	dw_4.Find(	"lotes 			= '" +	 	 ls_nrolote 	+ 	"' AND " 	+ &		
							    		"cama_codigo 	= " + string(li_camara) 	+	" AND " 	+ &
							    		"enva_tipoen 	= " + string(li_tipoenva) 	+ 	" AND " 	+ &
								 		"enva_codigo 	= " + string(li_envase),1, dw_4.RowCount())
														 
		IF ll_row<=0 THEN
		
			ll_row 		=	dw_4.RowCount()	
			
			li_Bultos	=	dw_3.Object.caex_canbul[ll_fila]
			
			dw_4.Object.lotes[ll_row]				=	ls_Nrolote
			dw_4.Object.lote_pltcod[ll_row]  	=  integer(mid(ls_nrolote,1,4))
			dw_4.Object.lote_espcod[ll_row]  	=  integer(mid(ls_nrolote,5,2))
			dw_4.Object.lote_codigo[ll_row]  	=  integer(mid(ls_nrolote,7,4))
			dw_4.Object.variedad[ll_row]			=	istr_mant.argumento[10]
			dw_4.Object.enva_tipoen[ll_row]		=	li_tipoenva
			dw_4.Object.enva_codigo[ll_row]  	=  li_envase
			dw_4.Object.enva_nombre[ll_row]  	=  dw_3.Object.enva_nombre[ll_fila]
			dw_4.Object.cama_codigo[ll_row]		=	li_camara
			dw_4.Object.mfgd_bulent[ll_row]		=	li_Bultos
			dw_4.Object.bultosdesp[ll_row]		=	li_Bultos
			ld_KilNet									=	li_Bultos * dw_3.Object.lfcd_kilnet[ll_Fila]
			dw_4.Object.mfgd_kgnent[ll_row]		=	ld_KilNet
			dw_4.Object.clie_codigo[ll_row]		=	li_Cliente
			ll_row 										=	dw_4.InsertRow(0)	
		END IF	
	NEXT	

	//Revisa los registros desde Detalle hacia Existencia, para Eliminación
	FOR  ll_fila=1 TO dw_4.Rowcount()
 	
		ls_nrolote	= dw_4.Object.lotes[ll_fila]
		li_tipoenva	= dw_4.Object.enva_tipoen[ll_fila]
		li_envase	= dw_4.Object.enva_codigo[ll_fila]
		li_camara	= dw_4.Object.cama_codigo[ll_fila]
	   
		ll_row	=	dw_3.Find(  "numero_lote 	= '" + 		 ls_nrolote 	+ "' AND " + &
							    		"cama_codigo 	= " + string(li_camara) 	+ " AND " + &
							    		"enva_tipoen 	= " + string(li_tipoenva)  + " AND " + &
								 		"enva_codigo 	= " + string(li_envase),1, dw_3.RowCount())
								 
   	IF ll_row<=0 THEN
			dw_4.deleterow(ll_fila)		
		END IF	
	
	NEXT

ELSE
	
	//Revisa los registros desde Detalle hacia Existencia, para Nuevos Detalles
	FOR  ll_fila=1 TO dw_4.Rowcount()
 	
		ls_nrolote	= dw_4.Object.lotes[ll_fila]
		li_tipoenva = dw_4.Object.enva_tipoen[ll_fila]
		li_envase	= dw_4.Object.enva_codigo[ll_fila]
		li_camara	= dw_4.Object.cama_codigo[ll_fila]
		li_pltcod 	= Integer(Mid(ls_nrolote,1,4))
		li_espcod 	= Integer(Mid(ls_nrolote,5,2))
		li_lotcod 	= Integer(Mid(ls_nrolote,7,4))
		
		ll_row	=	dw_3.Find(	"numero_lote 	= '" 	+ 			 ls_nrolote 	+ 	"' AND " + &
							    	 	"clie_codigo 	= " 	+ 	string(li_Cliente) 	+ 	" AND " + &		
							       	"cama_codigo 	= " 	+ 	string(li_camara) 	+ 	" AND " + &
							       	"enva_tipoen 	= " 	+ 	string(li_tipoenva)  + 	" AND " + &
								    	"enva_codigo 	= "	+ 	string(li_envase),1, dw_3.RowCount())
								 
   	IF ll_row<=0 THEN
		
			ll_row = dw_3.insertrow(0)
			
			li_Bultos	=	dw_4.Object.mfgd_bulent[ll_fila] 
			
			IF dw_4.Object.mfgd_kgnent[ll_fila] > 0 AND li_Bultos > 0 THEN
				ld_KilNet	=	dw_4.Object.mfgd_kgnent[ll_fila] / li_Bultos
			ELSE
				ld_KilNet	=	0
			END IF
		
      	dw_3.Object.lote_pltcod[ll_row]		=	li_pltcod
			dw_3.Object.lote_espcod[ll_row]		=	li_espcod
			dw_3.Object.lote_codigo[ll_row]		=	li_lotcod
			dw_3.Object.enva_tipoen[ll_row]		=	li_tipoenva
			dw_3.Object.enva_codigo[ll_row]  	=  li_envase
			dw_3.Object.enva_nombre[ll_row]  	=  dw_4.Object.enva_nombre[ll_fila]
			dw_3.Object.cama_codigo[ll_row]		=	li_camara
			dw_3.Object.caex_canbul[ll_row]		=	li_Bultos
			dw_3.Object.lotd_kilpro[ll_row]		=	ld_KilNet
			dw_3.Object.clie_codigo[ll_row]		=	li_Cliente
		   insertaexistencia(li_camara,ls_nrolote,li_tipoenva,li_envase,ll_row)
		
		END IF	
	NEXT
	
	//Revisa los registros desde Existencia hacia Detalle, para Eliminación
	FOR  ll_fila=1 TO dw_3.Rowcount()
 	
		ls_nrolote	= dw_3.Object.numero_lote[ll_fila]
		li_tipoenva = dw_3.Object.enva_tipoen[ll_fila]
		li_envase	= dw_3.Object.enva_codigo[ll_fila]
		li_camara	= dw_3.Object.cama_codigo[ll_fila]
	   
		ll_row	=	dw_4.Find(	"lotes 			= '" 	+ 			 ls_nrolote 	+	 "' AND " + &
							    		"clie_codigo 	= "	+ 	string(li_Cliente) 	+ 	" AND " + &		
							    		"cama_codigo 	= " 	+ 	string(li_camara) 	+	" AND " + &
							    		"enva_tipoen 	= " 	+ 	string(li_tipoenva)  + 	" AND " + &
								 		"enva_codigo 	= " 	+ 	string(li_envase),1, dw_4.RowCount())
								 
   	IF ll_row<=0 THEN
			dw_3.deleterow(ll_fila)		
		END IF	
	
	NEXT
	
END IF

end subroutine

public subroutine habilitaencab (boolean habilita);If Habilita Then
	dw_2.Object.b_bucaordenproceso.Visible    	=  1
	
	dw_2.Object.clie_codigo.Protect					=	0
	dw_2.Object.mfge_numero.Protect				=	0
	dw_2.Object.defg_docrel.Protect					=	0
	dw_2.Object.plde_coorde.Protect					=	0
	dw_2.Object.mfge_fecmov.Protect					=	0
	dw_2.Object.orpr_tipord.Protect					=	0
	
	dw_2.Object.clie_codigo.Color						=	0
	dw_2.Object.mfge_numero.Color					=	0
	dw_2.Object.defg_docrel.Color						=	0	
	dw_2.Object.plde_coorde.Color					=	0
	dw_2.Object.mfge_fecmov.Color					=	0
	
	dw_2.Object.clie_codigo.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.mfge_numero.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.defg_docrel.BackGround.Color		=	RGB(255,255,255)
	dw_2.Object.plde_coorde.BackGround.Color		=	RGB(255,255,255)	
	dw_2.Object.mfge_fecmov.BackGround.Color	=	RGB(255,255,255)
Else
	dw_2.Object.b_bucaordenproceso.Visible    	=  0
	
	dw_2.Object.clie_codigo.Protect					=	1
	dw_2.Object.mfge_numero.Protect				=	1
	dw_2.Object.defg_docrel.Protect					=	1
	dw_2.Object.plde_coorde.Protect					=	1
	dw_2.Object.mfge_fecmov.Protect					=	1
	dw_2.Object.orpr_tipord.Protect					=	1

	dw_2.Object.clie_codigo.Color						=	RGB(255,255,255)
	dw_2.Object.mfge_numero.Color					=	RGB(255,255,255)
	dw_2.Object.defg_docrel.Color						=	RGB(255,255,255)
	dw_2.Object.plde_coorde.Color					=	RGB(255,255,255)	
	dw_2.Object.mfge_fecmov.Color					=	RGB(255,255,255)
	
	dw_2.Object.clie_codigo.BackGround.Color		=	553648127
	dw_2.Object.mfge_numero.BackGround.Color	=	553648127
	dw_2.Object.defg_docrel.BackGround.Color		=	553648127
	dw_2.Object.plde_coorde.BackGround.Color		=	553648127
	dw_2.Object.mfge_fecmov.BackGround.Color	=	553648127
End If
end subroutine

public function boolean existedespacho (integer ai_planta, integer ai_tipomovto, long al_numero);Long		ll_Numero, ll_tipo
Boolean	lb_Retorno = True
Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.Argumento[16])

SELECT	defg_docrel, defg_tipdoc
	INTO	:ll_Numero, :ll_tipo
	FROM	dbo.spro_movtofrutagranenca
	WHERE	plde_codigo	=	:ai_Planta
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	mfge_numero	=	:al_Numero
	AND   clie_codigo =  :li_Cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Despachos Fruta Granel")

	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
	IF ll_tipo=4 OR ll_tipo=8 THEN
		IF ll_numero=0 or isnull(ll_numero) THEN
			MessageBox("Atención","Número de Orden de Proceso No ha sido Correctamente generado.")
			lb_Retorno	=	False
		ELSE
			istr_mant.Argumento[3]	=	String(al_Numero)
			This.TriggerEvent("ue_recuperadatos")
		END IF
	ELSE
		MessageBox("Atención","El Número de Movimiento No es del Tipo Traspaso a Proceso. Ingrese Otro.")
		lb_Retorno	=	False
	END IF 	
ELSE
	MessageBox("Atención","Número de Correlativo No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function boolean duplicado (string as_columna, string as_valor);String 	ls_lotes
Integer	li_tipoen, li_envase, li_camara, li_Cliente
Long		ll_fila

ls_lotes 	=	dw_4.Object.lotes[il_fila]
li_tipoen 	= 	dw_4.Object.enva_tipoen[il_fila]
li_envase 	= 	dw_4.Object.enva_codigo[il_fila]
li_camara 	= 	dw_4.Object.cama_codigo[il_fila]
li_Cliente	=	Integer(istr_mant.argumento[16])

CHOOSE CASE as_columna
	
	CASE 'lotes'
	     ls_lotes = as_valor
		  
	CASE 'enva_tipoen'
	     li_tipoen = integer(as_valor)
		  
   CASE 'enva_codigo'
	     li_envase = integer(as_valor)
		  
   CASE 'cama_codigo'
	     li_camara = integer(as_valor)
		  
END CHOOSE		  

IF isnull(ls_lotes) or ls_lotes="" or isnull(li_tipoen) or isnull(li_envase) or &
   isnull(li_camara) THEN
	RETURN False
ELSE
	ll_fila = dw_4.Find("lotes = '" + ls_lotes + "' and " + &
	                    "clie_codigo = " + string(li_cliente) + " And " + &	
	                    "enva_tipoen = " + string(li_tipoen) + " And " + &
	                    "enva_codigo = " + string(li_envase) + " And " + &
	                    "cama_codigo = " + string(li_camara), 1,dw_4.RowCount())
						  
	IF ll_fila > 0 and ll_fila <> il_fila THEN
		MessageBox("Error","El Registro ya Fue Ingresado",Information!, OK!)
   	RETURN True
	ELSE
	
		RETURN False
	END IF
END IF	

end function

public subroutine habilitaingreso (string as_columna);Boolean	lb_Estado = True
Date		ldt_Fecha

IF as_Columna <> "defg_docrel" AND &
	(dw_2.Object.defg_docrel[1] = 0 OR IsNull(dw_2.Object.defg_docrel[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "plde_coorde" AND &
	(dw_2.Object.plde_coorde[1] = 0 OR IsNull(dw_2.Object.plde_coorde[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "mfge_fecmov" AND &
	(dw_2.Object.mfge_fecmov[1] = ldt_Fecha OR IsNull(dw_2.Object.mfge_fecmov[1])) THEN
	lb_Estado = False
END IF

tab_1.tp_1.Enabled	=	lb_Estado
//tab_1.tp_2.Enabled	=	lb_Estado

//IF dw_4.Rowcount() = 0 AND lb_estado THEN
IF lb_estado THEN
	InsertadetalleGranel()
END IF

pb_ins_det.Enabled	=	lb_Estado
pb_eli_det.Enabled   =  lb_estado
pb_grabar.Enabled		=	lb_estado
end subroutine

public subroutine buscaenvase ();str_busqueda	lstr_busq
String			ls_Nula

SetNull(ls_Nula)

lstr_busq.argum[1]	=	String(dw_4.Object.enva_tipoen[il_Fila])

OpenWithParm(w_busc_envases, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	dw_4.SetColumn("enva_codigo")
	dw_4.Object.enva_codigo[dw_4.getrow()]	=	Integer(ls_Nula)
	dw_4.Object.enva_nombre[dw_4.getrow()]	=	ls_Nula
	dw_4.SetFocus()
ELSE
	dw_4.Object.enva_tipoen[dw_4.getrow()]	=	Integer(lstr_busq.argum[1])
	dw_4.Object.enva_codigo[dw_4.getrow()]	=	Integer(lstr_busq.argum[2])
	dw_4.Object.enva_nombre[dw_4.getrow()]	=	lstr_busq.argum[3]
END IF

RETURN
end subroutine

public function boolean noexistecliente (integer cliente);String ls_nombre

SELECT 	clie_nombre
INTO		:ls_nombre
FROM 		dbo.clientesprod
WHERE		clie_codigo =:cliente;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla ClientesProd")
	Return True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código No ha sido Generado. Ingrese Otro.")
	Return True
END IF

Return False
end function

public function boolean historial ();Boolean			lb_retorno	=	True
Integer			ll_fila
DwItemStatus	li_dwitemstatus
Integer			li_cliente, li_tipord, li_codmod, li_tipmov
Long				ll_planta, ll_proceso
Date 				ld_fecmov
Time				lt_hormov
String 			ls_pcname

ll_fila = 1
	
li_dwitemstatus	=	dw_2.GetItemStatus(ll_Fila, 0, Primary!)

IF li_dwitemstatus <> New! AND li_dwitemstatus <> NotModified! THEN
	li_cliente		=	Integer(istr_mant.argumento[16])
	ll_planta		=	dw_2.Object.plde_codigo[ll_fila]
	li_tipord		=	dw_2.Object.orpr_tipord[ll_fila]
	ll_proceso		=	dw_2.Object.defg_docrel[ll_fila]
	ld_fecmov		=	Date(f_fechahora())
	lt_hormov		=	Time(f_fechahora())
	ls_pcname		=	gstr_us.computador
			
	li_tipmov	=	2//2 traspaso a procesos
	
	CHOOSE CASE li_dwitemstatus
		CASE NewModified!
			li_codmod	=	1//1 creacion, 2 modificacion, 3 eliminacion
			
		CASE DataModified!
			li_codmod	=	2//1 creacion, 2 modificacion, 3 eliminacion
			
	END CHOOSE
	
	lb_retorno	=	iuo_historico.InsertaHistoria(li_cliente, ll_planta, li_tipord, &
																ll_proceso, li_codmod, li_tipmov, &
																ld_fecmov,  lt_hormov, ls_pcname, &
																True, Sqlca)
END IF

IF NOT lb_retorno THEN Return lb_retorno

dw_2.RowsCopy(1, dw_2.DeletedCount(), Delete!, dw_10, 1, Primary!)

IF dw_10.RowCount() > 0 THEN		
	li_cliente		=	Integer(istr_mant.argumento[16])
	ll_planta		=	dw_10.Object.plde_codigo[ll_fila]
	li_tipord		=	dw_10.Object.orpr_tipord[ll_fila]
	ll_proceso		=	dw_10.Object.defg_docrel[ll_fila]
	ld_fecmov		=	Date(f_fechahora())
	lt_hormov		=	Time(f_fechahora())
	ls_pcname		=	gstr_us.computador
			
	li_tipmov		=	2//2 traspaso a procesos
	li_codmod		=	3//1 creacion, 2 modificacion, 3 eliminacion
	
	lb_retorno	=	iuo_historico.InsertaHistoria(li_cliente, ll_planta, li_tipord, &
																ll_proceso, li_codmod, li_tipmov, &
																ld_fecmov,  lt_hormov, ls_pcname, &
																True, Sqlca)
	dw_10.Reset()
	
END IF

Return lb_retorno
end function

public function boolean procesopacking ();integer 	li_clie_codigo, li_i, li_hora, li_mins, li_mfge_numero, li_tipo, li_planpro, li_control
date 		ld_date_actual

//---------------variables para spro_ordenprocvacdeta
String 	ls_cale_calida
Decimal	ld_KilosRecep, ld_Tara, ld_NetoOriginal, ld_Neto, ld_KilosPromedio
time 		lt_hora_ivaciado, lt_hora_tvaciado

//---------------variables para lotefrutagranel
integer 	li_vari_codigo, li_prod_codigo
Long		ll_prod_codigo

//---------------variables para lotefrutagrandeta
integer 	li_plde_codigo, li_lote_espcod, li_lote_codigo, li_enva_tipoen, &
			  li_enva_codigo, li_lotd_totbul, li_lotd_totnet, li_lotd_kilpro
			  
//---------------variables para orden de proceso 
integer 	li_orpr_tipoord, li_orpr_numero

//---------------variables para programa de proceso 
integer 	li_ppre_numero, li_ppre_feccre

li_clie_codigo 	= 	dw_2.Object.clie_codigo[1]
li_mfge_numero 	= 	dw_2.Object.mfge_numero[1]
li_tipo 				= 	dw_2.Object.tpmv_codigo[1]
li_planpro			= 	dw_2.Object.plde_codigo[1]

dw_lotpro.Retrieve(li_planpro, li_tipo, li_mfge_numero, li_clie_codigo)

IF dw_lotpro.RowCount() < 1 THEN Return False


li_plde_codigo 	= 	dw_lotpro.Object.lote_pltcod[1]
li_lote_espcod		= 	dw_lotpro.Object.lote_espcod[1]
li_lote_codigo		= 	dw_lotpro.Object.lote_codigo[1]
li_enva_tipoen		= 	dw_lotpro.Object.enva_tipoen[1]
li_enva_codigo		= 	dw_lotpro.Object.enva_codigo[1]
li_lotd_totbul		= 	dw_lotpro.Object.lotd_totbul[1]
li_lotd_totnet		= 	dw_lotpro.Object.lotd_totnet[1]
li_lotd_kilpro		= 	dw_lotpro.Object.lotd_kilpro[1]

SELECT	vari_codigo, prod_codigo, lote_totnet
	INTO	:li_vari_codigo, :ll_prod_codigo, :ld_kilosRecep
	FROM	dbo.spro_lotesfrutagranel as lote
	WHERE 	lote.lote_pltcod 	= :li_plde_codigo
		AND 	lote.lote_espcod	= :li_lote_espcod
		AND 	lote.lote_codigo	= :li_lote_codigo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla LotesFrutaGranel")
	Return FALSE
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Encabezado del lote no ha sido generado, se aborta el proceso. Ingrese Otro.")
	Return FALSE
END IF

ld_date_actual 	= 	DATE(STRING(TODAY(), "dd/mm/yyyy"))
li_hora 				= 	Hour(now())
li_mins 				= 	Minute(now())
lt_hora_ivaciado 	= 	time(li_hora, li_mins,0)
lt_hora_tvaciado 	= 	time(li_hora + 1, li_mins,0)

//VAlida que el proceso se grabe solo la primera vez
select count(*) into :li_control
  from dbo.spro_movtofrutaembaenca 
 where clie_codigo	= :li_clie_codigo	
 	and plde_codigo	= :li_plde_codigo
	and tpmv_codigo 	= 4
	and mfee_numero	= :li_lote_codigo;
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutaembaenca")
	Return FALSE
ELSEIF li_control > 0 THEN
	Return True
END IF

//*********************************** tablas de encabezados

INSERT INTO dbo.spro_movtofrutaembaenca  
		( clie_codigo, plde_codigo, tpmv_codigo,   
		  mfee_numero, mfee_fecmov, plde_coorde,   
		  tran_codigo, cami_clasifi, cami_patent,   
		  cami_patcar, mfee_chofer, mfee_tipdoc,   
		  mfee_docrel, moti_codigo, puer_codigo,   
		  embq_codigo, mfee_guides, mfee_horsal,   
		  mfee_horarr, etiq_codigo, etiq_codnue,   
		  expo_codigo, mfee_nturno, mfee_plasag,   
		  mfee_termog, mfee_observ, mfee_cantar,   
		  mfee_tardef, mfee_cancaj, mfee_cajmer,   
		  mfee_usuari, mfee_comput, mfee_horacr,   
		  mfee_usumod, mfee_commod, mfee_horact,   
		  tica_codigo, mfee_rutcho, pcfl_numero,   
		  line_codigo, mfee_palins )  
VALUES ( :li_clie_codigo, :li_plde_codigo, 4,   
		  :li_lote_codigo, :ld_date_actual, null,   
		  null, null, null,   
		  null, null, 4,   
		  :li_lote_codigo, null, null,   
		  null, null, null,   
		  null, null, null,   
		  1, null, null,   
		  null, null, null,   
		  null, null, null,   
		  null, null, null,   
		  null, null, :lt_hora_ivaciado,   
		  null, null, null,   
		  null, null )  ;
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutaembaenca")
	Return FALSE
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
	Return FALSE
END IF

//*********************************** tablas de detalles
For Li_i = 1 to dw_lotpro.RowCount()	
	li_hora = Hour(now())
	li_mins = Minute(now())
	lt_hora_ivaciado = time(li_hora, li_mins + li_i,0)
	lt_hora_tvaciado = time(li_hora + 1, li_mins + li_i,0)
	
	li_plde_codigo = dw_lotpro.Object.lote_pltcod[li_i]
	li_lote_espcod	= dw_lotpro.Object.lote_espcod[li_i]
	li_lote_codigo	= dw_lotpro.Object.lote_codigo[li_i]
	li_enva_tipoen	= dw_lotpro.Object.enva_tipoen[li_i]
	li_enva_codigo	= dw_lotpro.Object.enva_codigo[li_i]
	li_lotd_totbul	= dw_lotpro.Object.lotd_totbul[li_i]
	li_lotd_totnet	= dw_lotpro.Object.lotd_totnet[li_i]
	li_lotd_kilpro	= dw_lotpro.Object.lotd_kilpro[li_i]

	
	INSERT INTO dbo.spro_ordenprocvacenca  
		( plde_codigo, orpr_tipord, orpr_numero,   
		  clie_codigo, opve_fecvac, opve_turno,   
		  opve_estado, line_codigo )  
	VALUES ( :li_plde_codigo, 4, :li_lote_codigo,   
	  	 :li_clie_codigo, :ld_date_actual, 1,   
	  	 'V', 1 )  ;
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenprocvacenca")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF
	
	IF li_clie_codigo = 81 THEN
		ls_cale_calida = "1"
	ELSE
		ls_cale_calida = "2"
	END IF
	
	
	INSERT INTO dbo.spro_terminoproceso  
			( plde_codigo, tpmv_codigo, tepr_numero,   
			  tepr_secuen, lote_pltcod, lote_espcod,   
			  lote_codigo, lote_secuen, enva_tipoen,   
			  enva_codigo, clie_codigo, tepr_bultra,   
			  tepr_bulvac )  
	VALUES (:li_plde_codigo, 21, :li_lote_codigo,   
			  1, :li_plde_codigo, :li_lote_espcod,   
			  :li_lote_codigo, :Li_i, :li_enva_tipoen,   
			  :li_enva_codigo, :li_clie_codigo, :li_lotd_totbul,   
			  :li_lotd_totbul )  ;
			  
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_terminoproceso")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF	
	
	ld_NetoOriginal	=	ld_KilosRecep
	ld_Neto				=	ld_NetoOriginal
	ld_KilosPromedio	=	ld_Neto / li_lotd_totbul
	ld_KilosRecep		=	ld_KilosRecep + ( ld_tara * li_lotd_totbul )
	
	INSERT INTO dbo.spro_ordenprocvacdeta  
		( plde_codigo, orpr_tipord, orpr_numero,   
		  clie_codigo, opve_fecvac, opve_turno,   
		  opvd_horava, opvd_horate, lote_pltcod,   
		  lote_espcod, lote_codigo, enva_tipoen,   
		  enva_codigo, cale_calida, opvd_canbul,
		  opvd_kilpro, opvd_kilori, opvd_pesobr, opvd_pesone)  
	VALUES ( :li_plde_codigo, 4, :li_lote_codigo,   
	  :li_clie_codigo, :ld_date_actual, 1,   
	  :lt_hora_ivaciado, :lt_hora_tvaciado, :li_plde_codigo,   
	  :li_lote_espcod, :li_lote_codigo, :li_enva_tipoen,   
	  :li_enva_codigo, :ls_cale_calida, :li_lotd_totbul,
	  :ld_KilosPromedio, :ld_NetoOriginal, :ld_KilosRecep, :ld_Neto)  ;
	  
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenprocvacdeta")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF


	UPDATE dbo.spro_ordenproceso  
   SET orpr_estado = 2  
	WHERE ( dbo.spro_ordenproceso.plde_codigo = :li_plde_codigo ) AND  
         ( dbo.spro_ordenproceso.clie_codigo = :li_clie_codigo ) AND  
			( dbo.spro_ordenproceso.orpr_tipord = 4 ) AND  
         ( dbo.spro_ordenproceso.orpr_numero = :li_lote_codigo ) ;
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_ordenproceso")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF
	
	li_enva_codigo	=	dw_2.Object.tpmv_codigo[1]
	li_lote_codigo	=	dw_2.Object.mfge_numero[1]
	
	UPDATE dbo.spro_movtofrutagranenca 
      SET mfge_estmov = 3  
   WHERE ( dbo.spro_movtofrutagranenca.plde_codigo = :li_plde_codigo ) AND  
         ( dbo.spro_movtofrutagranenca.clie_codigo = :li_clie_codigo ) AND  
         ( dbo.spro_movtofrutagranenca.tpmv_codigo = :li_enva_codigo ) AND  
         ( dbo.spro_movtofrutagranenca.mfge_numero = :li_lote_codigo );
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_movtofrutagranenca")
		Return FALSE
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención","Error, contacte al administrador del Sistema. Ingrese Otro.")
		Return FALSE
	END IF
	
NEXT 

RETURN TRUE
end function

event open;/* 
Argumentos
----------
istr_Mant.Argumento[1]	=	Código Planta
istr_Mant.Argumento[2]	=	Tipo de Movimiento
istr_Mant.Argumento[3]	=	Número de Despacho a Proceso
istr_Mant.Argumento[4]	=	Tipo Dcto Relacionado
istr_Mant.Argumento[5]	=	Orden de Proceso (doc. relacionado)
istr_Mant.Argumento[6]	=	Codigo Productor
istr_Mant.Argumento[7]	=	Estado del Movimiento
istr_Mant.Argumento[8]	=	Codigo Especie
istr_Mant.Argumento[9]	=	Codigo Variedad
istr_Mant.Argumento[10]	=	Nombre Variedad
istr_Mant.Argumento[11]	=	Cantidad en Orden de Proceso
istr_Mant.Argumento[12]	=	Codigo de Tratamiento Frío
istr_Mant.Argumento[13]	=	Codigo de Periodo Frío
istr_Mant.Argumento[14]	=	Turno
istr_Mant.Argumento[15]	=	Planta Coorde
istr_Mant.Argumento[16]	=	Código Cliente 
*/

Integer	li_TipoEnvase

istr_Mant.Argumento[1]	=	""
istr_Mant.Argumento[2]	=	""
istr_Mant.Argumento[3]	=	""
istr_Mant.Argumento[4]	=	""
istr_Mant.Argumento[5]	=	""
istr_Mant.Argumento[6]	=	""
istr_Mant.Argumento[7]	=	""
istr_Mant.Argumento[8]	=	""
istr_Mant.Argumento[9]	=	""
istr_Mant.Argumento[10]	=	""
istr_Mant.Argumento[11]	=	""
istr_Mant.Argumento[12]	=	""
istr_Mant.Argumento[13]	=	""
istr_Mant.Argumento[14]	=	""
istr_Mant.Argumento[15]	=	""
istr_Mant.Argumento[16]	=	String(gi_Codexport)

dw_4	=	tab_1.tp_1.dw_detalle
dw_5	=	tab_1.tp_2.dw_envases

dw_1.GetChild("plde_codigo", idwc_Plantadw1)
idwc_Plantadw1.SetTransObject(sqlca)
idwc_Plantadw1.Retrieve(gi_codexport) 

dw_1.GetChild("plde_coorde", idwc_pladesdw1)
idwc_pladesdw1.SetTransObject(sqlca)
idwc_pladesdw1.Retrieve(gi_codexport) 


dw_2.GetChild("plde_codigo", idwc_Planta)
idwc_Planta.SetTransObject(sqlca)
idwc_Planta.Retrieve(gi_codexport) 
idwc_Planta.InsertRow(0)

dw_2.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
//idwc_productor.Retrieve(-1) 
idwc_productor.Retrieve(gi_codexport) 
idwc_productor.InsertRow(0)

dw_2.GetChild("pefr_codigo", idwc_periodo)
idwc_periodo.SetTransObject(sqlca)
idwc_periodo.Retrieve() 
idwc_periodo.InsertRow(0)

dw_2.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve(Integer(istr_Mant.Argumento[16])) 
idwc_especie.InsertRow(0)

dw_2.GetChild("tpmv_codigo", idwc_tipo)
idwc_tipo.SetTransObject(sqlca)
idwc_tipo.Retrieve() 
idwc_tipo.InsertRow(0)

dw_2.GetChild("plde_coorde", idwc_Packing)
idwc_Packing.SetTransObject(sqlca)

IF idwc_Packing.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención", "Falta Registrar Packing para Planta " + &
					String(gstr_ParamPlanta.CodigoPlanta, '0000'))
	idwc_Packing.InsertRow(0)
ELSE
	idwc_packing.SetFilter("plde_tipopl in(1,3)")
	idwc_packing.Filter()
	idwc_Packing.SetSort("plde_nombre A")
	idwc_Packing.Sort()
END IF

dw_2.GetChild("line_codigo", idwc_Linea)
idwc_Linea.SetTransObject(sqlca)

IF idwc_Linea.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 THEN
	MessageBox("Atención", "Falta Registrar Líneas para Packing " + &
					String(gstr_ParamPlanta.CodigoPlanta, '0000'))
	idwc_Linea.InsertRow(0)
ELSE
	idwc_Linea.SetSort("line_nombre A")
	idwc_Linea.Sort()
END IF

// Getchild para Detalle de Fruta
dw_4.GetChild("enva_tipoen", idwc_TipoEnvase1)
idwc_TipoEnvase1.SetTransObject(sqlca)

IF idwc_TipoEnvase1.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Envases ")
	idwc_TipoEnvase1.InsertRow(0)
ELSE
	idwc_TipoEnvase1.SetFilter("tien_usoenv = 1")
	idwc_TipoEnvase1.Filter()
	IF idwc_TipoEnvase1.RowCount() > 0 THEN
		li_TipoEnvase	=	idwc_TipoEnvase1.GetItemNumber(1,"enva_tipoen")
	ELSE
		MessageBox("Atención", "Falta Registrar Envases Cosecheros")
		idwc_TipoEnvase1.InsertRow(0)
	END IF		
	idwc_TipoEnvase1.SetSort("tien_nombre A")
	idwc_TipoEnvase1.Sort()
END IF

dw_4.GetChild("enva_codigo", idwc_Envases1)
idwc_Envases1.SetTransObject(sqlca)
//IF idwc_Envases1.Retrieve(li_TipoEnvase) = 0 THEN
//	MessageBox("Atención", "Falta Registrar los Envases")
//	idwc_Envases1.InsertRow(0)
//END IF

dw_4.GetChild("cama_codigo", idwc_Camara1)
idwc_Camara1.SetTransObject(sqlca)

IF idwc_Camara1.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 THEN
	MessageBox("Atención", "Falta Registrar Cámaras para Planta " + &
					String(gstr_ParamPlanta.CodigoPlanta, '0000'))
	idwc_Camara1.InsertRow(0)
ELSE
	idwc_Camara1.SetSort("cama_nombre A")
	idwc_Camara1.Sort()
END IF

// Getchild para Detalle de Envases
dw_5.GetChild("enva_tipoen", idwc_TipoEnvase)
idwc_TipoEnvase.SetTransObject(sqlca)

IF idwc_TipoEnvase.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Envases ")
	idwc_TipoEnvase.InsertRow(0)
ELSE
	idwc_TipoEnvase.SetSort("tien_nombre A")
	idwc_TipoEnvase.Sort()
END IF

dw_5.GetChild("cama_codigo", idwc_Camara)
idwc_Camara.SetTransObject(sqlca)

IF idwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 THEN
	MessageBox("Atención", "Falta Registrar Cámaras para Planta " + &
					String(gstr_ParamPlanta.CodigoPlanta, '0000'))
	idwc_Camara.InsertRow(0)
ELSE
	idwc_Camara.SetSort("cama_nombre A")
	idwc_Camara.Sort()
END IF
////////////            //////////////////

dw_3.GetChild("cama_codigo", idwc_Camara2)
idwc_Camara2.SetTransObject(sqlca)

IF idwc_Camara2.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 THEN
	idwc_Camara2.InsertRow(0)
ELSE
	idwc_Camara2.SetSort("cama_nombre A")
	idwc_Camara2.Sort()
END IF

Call Super::Open

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	Message.StringParm
istr_Mant.Argumento[3]	=	""
istr_Mant.Argumento[4]	=	"4"
istr_Mant.Argumento[7]	=	"1"
ii_Orpr_TipOrd				=	4

dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_1.SetTransObject(sqlca)
dw_lotpro.SetTransObject(sqlca)

dw_4.Modify("datawindow.message.title='Error '+ is_titulo")
dw_4.SetRowFocusIndicator(Hand!)
dw_4.Modify("DataWindow.Footer.Height = 88")

dw_5.Modify("datawindow.message.title='Error '+ is_titulo")
dw_5.SetRowFocusIndicator(Hand!)
dw_5.Modify("DataWindow.Footer.Height = 88")

iuo_Packing				=	Create uo_plantadesp
iuo_envases				=	Create uo_envases
iuo_lotes				=	Create uo_lotesfrutagranel
iuo_camara				=	Create uo_camarasfrigo
iuo_tipomovtofruta	=	Create uo_tipomovtofruta
iuo_TipoMovtoEnva		=	Create uo_tipomovtofruta
iuo_historico			=	Create uo_control_historico_proceso

ids_detalle			=  Create Datastore
ids_detalle.dataobject= "dw_mant_mues_detamovto_proceso"
ids_detalle.SetTransObject(sqlca)


// Getchild para Detalle de Fruta datastore
ids_detalle.GetChild("enva_tipoen", ids_tipo)
ids_tipo.SetTransObject(sqlca)

IF ids_tipo.Retrieve() = 0 THEN
	idwc_TipoEnvase1.InsertRow(0)
ELSE
	ids_tipo.SetFilter("tien_usoenv = 1")
	ids_tipo.Filter()
	ids_tipo.SetSort("tien_nombre A")
	ids_tipo.Sort()
END IF

ids_detalle.GetChild("enva_codigo", ids_envase)
ids_envase.SetTransObject(sqlca)
IF ids_envase.Retrieve(li_TipoEnvase) = 0 THEN
	ids_envase.InsertRow(0)
END IF

ids_detalle.GetChild("cama_codigo", ids_camara)
ids_camara.SetTransObject(sqlca)

IF ids_camara.Retrieve(gstr_ParamPlanta.CodigoPlanta) = 0 THEN
	ids_camara.InsertRow(0)
ELSE
	ids_camara.SetSort("cama_nombre A")
	ids_camara.Sort()
END IF

This.Height	=	2500
end event

event ue_borra_detalle();call super::ue_borra_detalle;SetPointer(HourGlass!)

ib_Borrar				=	True
Message.DoubleParm	=	0
istr_mant.Borra		=	True
istr_mant.Agrega		=	False

w_main.SetMicroHelp("Validando la eliminación de detalle...")

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF tab_1.SelectedTab = 1 THEN
	istr_Mant.dw	=	dw_4
	
	//OpenWithParm(iw_mantencion_1, istr_Mant)
	
	//istr_Mant	=	Message.PowerObjectParm
	
	//IF istr_mant.Respuesta = 1 THEN
		IF dw_4.DeleteRow(0) = 1 THEN
			ib_Borrar	=	False
			
			w_main.SetMicroHelp("Borrando Registro...")
			
			ib_borra	=	False
			SetPointer(Arrow!)
		ELSE
			ib_Borrar	=	False
			
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	
		IF dw_4.RowCount() = 0 THEN 
			HabilitaEncab(True)
			
			pb_eli_det.Enabled			=	False
		END IF
	//END IF
ELSE
	istr_Mant.dw	=	dw_5
	
	OpenWithParm(iw_mantencion_2, istr_Mant)
	
	istr_Mant	=	Message.PowerObjectParm
	
	IF istr_mant.Respuesta = 1 THEN
		IF dw_5.DeleteRow(0) = 1 THEN
			ib_Borrar	=	False
			
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			ib_Borrar	=	False
			
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	
		IF dw_5.RowCount() = 0 THEN 
			HabilitaEncab(True)
			pb_eli_det.Enabled	=	False
		END IF
	END IF
END IF

istr_mant.Borra	 = False
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;Integer li_row
Long  ll_suma


HabilitaEncab(False)

istr_mant.Borra	=	False
istr_mant.Agrega	=	True
	
IF tab_1.SelectedTab = 1 THEN
	
	il_fila = dw_4.InsertRow(0)	
	dw_4.Object.mfgd_secuen[il_Fila]	=	il_Fila
	dw_4.Object.variedad[il_fila]		=  istr_mant.argumento[10]
	dw_4.Object.lote_pltcod[il_fila] 	=  dw_2.Object.plde_codigo[1]
	dw_4.Object.lote_espcod[il_fila] 	=  dw_2.Object.espe_codigo[1]
	dw_4.Object.clie_codigo[il_fila]	=  Integer(istr_mant.argumento[16])
	dw_4.ScrollToRow(il_fila)
	dw_4.SetRow(il_fila)
	dw_4.SetColumn("lote_codigo")
	dw_4.SetFocus()
	
	
	IF dw_4.RowCount() > 0 THEN
		pb_eli_det.Enabled			=	True
	ELSE
		pb_eli_det.Enabled			=	False
	END IF
	
ELSE
	istr_mant.dw	=	dw_5

	OpenWithParm(iw_mantencion_2, istr_mant)

	IF dw_5.RowCount() > 0 THEN
		pb_eli_det.Enabled			=	True
	ELSE
		pb_eli_det.Enabled			=	False
	END IF
END IF

IF dw_4.RowCount() > 0 THEN HabilitaEncab(False)

IF dw_4.RowCount() > 0 AND Not pb_eliminar.Enabled THEN
	pb_eliminar.Enabled	=	NOT istr_Mant.Solo_Consulta
	pb_grabar.Enabled		=	NOT istr_Mant.Solo_Consulta
END IF


end event

event ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta, ll_row
String ls_lote

DO
	dw_2.SetRedraw(False)

	ll_fila_e	=	dw_2.Retrieve(Integer(istr_Mant.Argumento[1]), &
										  Integer(istr_Mant.Argumento[2]), &
 										  Long(istr_Mant.Argumento[3]), &
										  Integer(istr_Mant.Argumento[16]))
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila_e < 1 THEN 
		RETURN
	ELSE
		
		istr_mant.argumento[6]	=	String(dw_2.Object.prod_codigo[1])
		istr_mant.argumento[7]	=	String(dw_2.Object.mfge_estmov[1])
		istr_mant.argumento[8]	=	String(dw_2.Object.espe_codigo[1])
		istr_mant.argumento[9]	=	String(dw_2.Object.vari_codigo[1])
		istr_mant.argumento[10]	=	dw_2.Object.vari_nombre[1]
		istr_mant.argumento[12]	=	dw_2.Object.frio_tipofr[1]
		istr_mant.argumento[13]	=	String(dw_2.Object.pefr_codigo[1])
		istr_mant.argumento[14]	=	String(dw_2.Object.orpr_nrotur[1])
		istr_mant.argumento[5]	=	String(dw_2.Object.defg_docrel[1])
		
		IF dw_2.Object.mfge_estmov[1] <> 1 THEN
			istr_Mant.Solo_Consulta	=	True
		ELSE
			istr_Mant.Solo_Consulta	=	False
		END IF
		
		DO
			IF dw_4.Retrieve(Integer(istr_mant.argumento[1]), &
									Integer(istr_mant.argumento[2]), &
									Long(istr_mant.argumento[3]),Integer(istr_Mant.Argumento[16])) = -1 OR &
				ids_detalle.Retrieve(Integer(istr_mant.argumento[1]), &
									Integer(istr_mant.argumento[2]), &
									Long(istr_mant.argumento[3]),Integer(istr_Mant.Argumento[16])) = -1 OR &					
				dw_1.Retrieve(Integer(istr_mant.argumento[1]), &
									Integer(istr_mant.argumento[2]), &
									Long(istr_mant.argumento[3]),Integer(istr_Mant.Argumento[16])) = -1 OR &
				dw_5.Retrieve(Integer(istr_mant.argumento[1]), &
									Integer(istr_mant.argumento[2]), &
									Long(istr_mant.argumento[3]), 2,Integer(istr_Mant.Argumento[16])) = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", &
												"No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				IF dw_4.RowCount()>0 THEN
					
					FOR ll_row=1 TO dw_4.RowCount()
						ls_lote	=	String(dw_4.Object.lote_pltcod[ll_row],'0000') + &
										String(dw_4.Object.lote_espcod[ll_row],'00') + &
										String(dw_4.Object.lote_codigo[ll_row],'0000')
						
						dw_4.Object.lotes[ll_row]		=	ls_lote
						dw_4.Object.variedad[ll_row]	=	istr_mant.Argumento[10]
						
						dw_4.Object.bultosdesp[ll_row]=	dw_4.Object.mfgd_bulent[ll_row]		 
						
						IF iuo_envases.Existe(dw_4.Object.enva_tipoen[ll_row],dw_4.Object.enva_codigo[ll_row], True, Sqlca) THEN
							dw_4.Object.enva_nombre[ll_row]=iuo_envases.Nombre	
						END IF
						dw_4.SetItemStatus(ll_row, 0, Primary!, NotModified!)
					NEXT
					il_secuencia=dw_4.Object.mfgd_secuen[dw_4.rowcount()]
				END IF	
				
				IF istr_Mant.Solo_Consulta THEN
				  dw_4.Object.lote_pltcod.Protect = 1
				  dw_4.Object.lote_codigo.Protect = 1
				  dw_4.Object.enva_tipoen.Protect = 1
				  dw_4.Object.enva_codigo.Protect = 1
				  dw_4.Object.cama_codigo.Protect = 1
				  dw_4.Object.bultosdesp.Protect  = 1
			   ELSE  
 				  dw_4.Object.lote_pltcod.Protect = 0
				  dw_4.Object.lote_codigo.Protect = 0
				  dw_4.Object.enva_tipoen.Protect = 0
				  dw_4.Object.enva_codigo.Protect = 0
				  dw_4.Object.cama_codigo.Protect = 0
				  dw_4.Object.bultosdesp.Protect  = 0	
				END IF	
				
				pb_eliminar.Enabled	=	NOT istr_Mant.Solo_Consulta
				pb_grabar.Enabled		=	NOT istr_Mant.Solo_Consulta
				pb_imprimir.Enabled	=	TRUE                       //NOT istr_Mant.Solo_Consulta
				pb_ins_det.Enabled	=	NOT istr_Mant.Solo_Consulta

				HabilitaEncab(False)
				
				pb_ins_det.Enabled	=	NOT istr_Mant.Solo_Consulta
				pb_eli_det.Enabled	=	NOT istr_Mant.Solo_Consulta
				
				dw_4.SetFocus()
				
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_maed_movtofrutagranel_proceso.create
int iCurrent
call super::create
this.tab_1=create tab_1
this.dw_3=create dw_3
this.dw_10=create dw_10
this.dw_lotpro=create dw_lotpro
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
this.Control[iCurrent+2]=this.dw_3
this.Control[iCurrent+3]=this.dw_10
this.Control[iCurrent+4]=this.dw_lotpro
end on

on w_maed_movtofrutagranel_proceso.destroy
call super::destroy
destroy(this.tab_1)
destroy(this.dw_3)
destroy(this.dw_10)
destroy(this.dw_lotpro)
end on

event ue_modifica_detalle();integer li_row
Long    ll_suma

istr_mant.Agrega	=	False
istr_mant.Borra	=	False

IF tab_1.SelectedTab = 1 THEN

		istr_Mant.Agrega	= False
		istr_Mant.Borra	= False
		istr_Mant.dw	=	dw_3
      
		Revisa_Registros(False)
		
		OpenWithParm(iw_Mantencion_1, istr_Mant)
		
		Revisa_Registros(True)
		//ingresaregistros()
ELSE
	IF dw_5.RowCount() > 0 THEN
		
		istr_mant.dw	=	dw_5
		
		OpenWithParm(iw_mantencion_2, istr_mant)
	END IF
END IF
end event

event ue_nuevo;Long		ll_modif
DateTime	ldt_Fecha

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif	=	dw_1.GetNextModified(0, Primary!)
			ll_modif	+=	dw_2.GetNextModified(0, Primary!)
			ll_modif	+=	dw_4.GetNextModified(0, Primary!)
			ll_modif	+=	dw_5.GetNextModified(0, Primary!)
		
			IF dw_4.RowCount() > 0 AND ll_Modif > 0 THEN
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
dw_3.reset()
ids_detalle.reset()
dw_4.Reset()
dw_5.Reset()

tab_1.tp_1.Enabled		=	False
tab_1.tp_2.Enabled		=	False
pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled		=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

ldt_Fecha	=	F_FechaHora()

dw_2.Object.plde_codigo[1]		=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.tpmv_codigo[1]	=	Integer(istr_Mant.Argumento[2])
dw_2.Object.mfge_fecmov[1]	=	DateTime(Date(ldt_Fecha))
dw_2.Object.defg_tipdoc[1]		=	ii_Orpr_TipOrd
dw_2.Object.plde_coorde[1]	=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.mfge_estmov[1]	=	1
dw_2.Object.clie_codigo[1]		=	Integer(istr_Mant.Argumento[16])
dw_2.SetItem(dw_2.GetRow(), "orpr_tipord",ii_Orpr_TipOrd)

istr_mant.argumento[1]     	= string(gstr_ParamPlanta.CodigoPlanta)
istr_mant.argumento[15] 	= 	string(gstr_ParamPlanta.CodigoPlanta)

dw_2.SetFocus()

end event

event ue_antesguardar;Long		ll_Fila, ll_row
Integer	li_Secuencia, li_Planta, li_TipoMovto,li_plt,li_esp, li_lot, &
			li_cama, li_tipoenva,li_envase, li_Bultos, li_Bultra, li_Cliente
Dec{3}	ld_KilNet
String   ls_lote
Boolean  lb_Continua=True, lb_Actualiza_Fruta = FALSE, lb_Actualiza_Envase = FALSE

li_Cliente	= Integer(istr_mant.Argumento[16])

dw_4.AcceptText()

////	Control de Envases 
IF (istr_mant.argumento[2]='21') and  (istr_mant.argumento[7]<>'1') THEN
//   IF (dw_4.RowCount()<1 or dw_5.RowCount()<1)  THEN
//	   MessageBox("Error de Consistencia", "Cantidad de Bultos Despachados debe " + &
//		 			"corresponder con Cantidad de Envases.~r~r" + &
//			 		"Corrija Detalle de Fruta o Detalle de Envases.")
//					
//	   Message.DoubleParm = -1
//		RETURN
//	END IF	
//	IF (dw_4.Object.totbultosdes[1] <> dw_5.Object.total_envases[1]) THEN
//		MessageBox("Error de Consistencia", "Cantidad de Bultos Despachados debe " + &
//					"corresponder con Cantidad de Envases.~r~r" + &
//					"Corrija Detalle de Fruta o Detalle de Envases.")
//					
//		Message.DoubleParm = -1
//	END IF
	FOR ll_fila=1 to dw_4.rowcount()

		IF dw_4.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THEN
			dw_4.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
			dw_4.Object.tpmv_codigo[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
			dw_4.Object.mfge_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]
			dw_4.Object.plde_coorde[ll_fila] =  dw_2.Object.plde_coorde[1]
		ELSEIF dw_4.GetItemStatus(ll_fila, 0, Primary!) = DataModified! THEN
			dw_2.Object.mfge_fecmov[1]			=	dw_2.Object.mfge_fecmov[1]
		END IF
		
		ld_KilNet								=	dw_4.Object.mfgd_kgnent[ll_Fila]
		li_Bultos								=	dw_4.Object.mfgd_bulent[ll_Fila]
		li_BulTra								=	dw_4.Object.bultosdesp[ll_fila]
		
		IF ld_KilNet > 0 AND li_Bultos > 0 THEN
			dw_4.Object.mfgd_kgnent[ll_Fila]	=	ld_KilNet / li_Bultos * li_Bultra
		ELSE
			dw_4.Object.mfgd_kgnent[ll_Fila]	=	0
		END IF
		
		dw_4.Object.mfgd_bulent[ll_Fila]	=	dw_4.Object.bultosdesp[ll_fila]
		
		li_Secuencia ++
	
	NEXT
	
	FOR ll_Fila = 1 TO dw_5.RowCount()
		IF dw_5.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
			dw_5.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
			dw_5.Object.tpmv_codigo[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
			dw_5.Object.meen_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]
			dw_5.Object.clie_codigo[ll_Fila]	=	li_Cliente
		END IF
	NEXT
ELSE
	
	IF dw_4.RowCount()<=0 THEN
		MessageBox("Error de Consistencia", "Debe Ingresar un Detalle de Traspaso.")
		Message.DoubleParm = -1
		lb_Continua	=	False
	END IF	
	
	ll_fila=1
   Do While ll_fila <= dw_4.rowcount()	

      IF isnull(dw_4.Object.lotes[ll_Fila]) OR dw_4.Object.lotes[ll_Fila] = "" OR &
		   isnull(dw_4.Object.enva_tipoen[ll_Fila]) OR isnull(dw_4.Object.enva_codigo[ll_Fila]) OR &
			isnull(dw_4.Object.cama_codigo[ll_Fila]) THEN
			MessageBox("Error de Consistencia", "Existen valores Nulos en el Registro " + string(ll_fila)+" .")
		
         Message.DoubleParm = -1
			ll_fila		=	dw_4.rowcount()+1
			lb_Continua	=	False

		ELSE

			IF dw_4.Object.mfgd_bulent[ll_Fila]	< dw_4.Object.bultosdesp[ll_fila] THEN
				MessageBox("Error de Consistencia", "Cantidad de Saldo Despachado debe " + &
						"ser menor o igual a la Cantidad de Bultos Requeridos.~r~r" + &
						"Corrija el Saldo Despachado.")
					
				Message.DoubleParm = -1
				ll_fila		=	dw_4.rowcount()+1
				lb_Continua	=	False
			ELSE
  				ll_fila++
			END IF
		END IF	
	LOOP
	
	
   IF lb_Continua THEN	
	
		ib_AutoCommit		=	sqlca.AutoCommit
		sqlca.AutoCommit	=	False
	
		li_Planta			=	dw_2.Object.plde_codigo[1]
		li_TipoMovto		=	dw_2.Object.tpmv_codigo[1]
				
		IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
			IF il_NumFruta=0 THEN
				iuo_TipoMovtoFruta.bloqueacorrel()
				il_NumFruta = iuo_TipoMovtoFruta.ultimocorrelativo(1,li_TipoMovto,li_Planta) 
			
				IF il_NumFruta = 0 OR IsNull(il_NumFruta) THEN
					Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
					Message.DoubleParm = -1
					RETURN
				ELSE
					lb_Actualiza_Fruta = TRUE	
				END IF
			END IF
		
			dw_2.Object.mfge_numero[1]	=	il_NumFruta
			dw_2.Object.mfge_estmov[1] =  1
			dw_2.Object.defg_tipdoc[1]	=	Integer(istr_Mant.Argumento[4])
			ll_Fila	=	dw_1.InsertRow(0)
			
			dw_1.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
			dw_1.Object.tpmv_codigo[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
			dw_1.Object.meen_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]
			dw_1.Object.plde_coorde[ll_Fila]	=	dw_2.Object.plde_coorde[1]
			dw_1.Object.prod_codigo[ll_Fila]	=	dw_2.Object.prod_codigo[1]
			dw_1.Object.meen_fecmov[ll_Fila]	=	dw_2.Object.mfge_fecmov[1]
			//dw_1.Object.tran_codigo[ll_Fila]	=  1 Ahora acepta nulo
			dw_1.Object.meen_modulo[ll_Fila] =	1
			dw_1.Object.tpmv_codrec[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
			dw_1.Object.mfge_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]
			dw_1.Object.clie_codigo[ll_Fila] =  li_Cliente
  			
				//Preguntar el Momento de Actualización
			IF lb_Actualiza_Fruta  THEN iuo_TipoMovtoFruta.Actualiza_Correlativo(1,li_Planta,li_TipoMovto,il_NumFruta) 
		      ///////////////////////////////////////
		ELSE	
		   il_NumFruta			=	dw_2.Object.mfge_numero[1]	
		END IF
		
		istr_mant.Argumento[3]	=	String(dw_2.Object.mfge_numero[1])

		SELECT	IsNull(Max(mfgd_secuen), 0) + 1
		  INTO	:li_Secuencia
		  FROM	dbo.spro_movtofrutagrandeta
		 WHERE	plde_codigo	=	:li_Planta
			AND	tpmv_codigo	=	:li_TipoMovto
			AND	mfge_numero	=	:il_NumFruta
			AND   clie_codigo =  :li_Cliente;
	
		FOR ll_fila=1 to dw_4.rowcount()

			IF dw_4.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THEN
				dw_4.Object.mfgd_secuen[ll_Fila] =  li_Secuencia
				dw_4.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
				dw_4.Object.tpmv_codigo[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
				dw_4.Object.mfge_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]
				dw_4.Object.plde_coorde[ll_fila] =  dw_2.Object.plde_coorde[1]
				dw_4.Object.clie_codigo[ll_fila] =  li_Cliente
			ELSEIF dw_4.GetItemStatus(ll_fila, 0, Primary!) = DataModified! THEN
				dw_2.Object.mfge_fecmov[1]			=	dw_2.Object.mfge_fecmov[1]
			END IF
			
			ld_KilNet								=	dw_4.Object.mfgd_kgnent[ll_Fila]
			li_Bultos								=	dw_4.Object.mfgd_bulent[ll_Fila]
			li_BulTra								=	dw_4.Object.bultosdesp[ll_fila]
			
			IF ld_KilNet > 0 AND li_Bultos > 0 THEN
				dw_4.Object.mfgd_kgnent[ll_Fila]	=	ld_KilNet / li_Bultos * li_Bultra
			ELSE
				dw_4.Object.mfgd_kgnent[ll_Fila]	=	0
			END IF
			
			dw_4.Object.mfgd_bulent[ll_Fila]	=	dw_4.Object.bultosdesp[ll_fila]
			
  		   li_Secuencia ++
		
		NEXT
	

 		FOR ll_Fila = 1 TO dw_5.RowCount()
			
			IF dw_5.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
				dw_5.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
				dw_5.Object.tpmv_codigo[ll_Fila]	=	dw_2.Object.tpmv_codigo[1]
				dw_5.Object.meen_numero[ll_Fila]	=	dw_2.Object.mfge_numero[1]
				dw_5.Object.clie_codigo[ll_Fila]	=	li_Cliente
			END IF
		NEXT
	END IF
END IF
 
//asignacion al cliente seleccionado en la ventana.
FOR ll_fila = 1 to dw_1.RowCount()
	dw_1.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[16])
NEXT

FOR ll_fila = 1 to dw_3.RowCount()
	dw_3.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[16])
NEXT

FOR ll_fila = 1 to dw_4.RowCount()
	dw_4.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[16])
NEXT

FOR ll_fila = 1 to dw_5.RowCount()
	dw_5.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[16])
NEXT

// Control de lotes Destarados
IF dw_4.RowCount() > 0 THEN
	FOR ll_fila = 1 TO dw_4.RowCount()
		ls_lote = dw_4.Object.lotes[ll_fila]
		IF Not lotesdestarados(ls_lote) THEN
			Messagebox("Error de Consistemncia","El lote " + ls_lote + " no ha sido destarado" + &
						  									"~r~rPor favor ingrese otro lote")
			Message.DoubleParm = -1
			RETURN
		END IF
	NEXT
END IF
end event

event ue_borrar;IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar		=	True
ib_AutoCommit	=	sqlca.AutoCommit

w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_4.RowCount() > 0 THEN
	dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, 1, Delete!)
END IF

IF dw_5.RowCount() > 0 THEN
	dw_5.RowsMove(1, dw_5.RowCount(), Primary!, dw_5, 1, Delete!)
END IF

IF dw_2.DeleteRow(0) = 1 AND dw_1.DeleteRow(0) = 1 THEN
		ib_Borrar	=	False
		
		w_main.SetMicroHelp("Borrando Registro...")
		
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
ELSE
	ib_Borrar	=	False
	
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF
end event

event ue_seleccion;Str_Busqueda	lstr_busq
Date				ld_FechaInicio
String ls_Nula
SetNull(ls_nula)

lstr_Busq.Argum[1]	=	istr_Mant.Argumento[1]
lstr_Busq.Argum[2]	=	istr_Mant.Argumento[2]
lstr_Busq.Argum[3]	=	'' // istr_mant.argumento[7]
lstr_Busq.Argum[4]	=	''
lstr_Busq.Argum[16]	=	istr_Mant.Argumento[16]

OpenWithParm(w_busc_spro_movtofrutagranenca, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	istr_Mant.Argumento[3]	=	lstr_Busq.Argum[3]
	il_eleccion=1
	IF NOT ExisteDespacho(gstr_ParamPlanta.CodigoPlanta, Integer(istr_mant.argumento[2]), Integer(lstr_Busq.Argum[3])) THEN
		dw_2.SetItem(1,"mfge_numero", Long(ls_Nula))
		dw_2.SetFocus()
	ELSE
		tab_1.tp_1.Enabled	=	True
		IF (istr_mant.argumento[2]='21') and  (istr_mant.argumento[7]<>'1') THEN
   		tab_1.tp_2.Enabled	=	True
	   END IF
   	//pb_ins_det.Enabled	=	True	
	END IF
END IF
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	IF gstr_paramplanta.semipacking THEN
		IF PROCESOPACKING() THEN
			dw_2.Object.mfge_estmov[1]	=	3
			dw_2.SetItemStatus(1, 0, Primary!, NotModified!)
			w_main.SetMicroHelp("Información Grabada.")
			pb_eliminar.Enabled			= 	True
			pb_imprimir.Enabled			= 	True
			
		ELSE
			w_main.SetMicroHelp("No se puede Grabar información.")
			Message.DoubleParm = -1
			RETURN
		END IF
		
	ELSE
		w_main.SetMicroHelp("Información Grabada.")
		pb_eliminar.Enabled	= True
		pb_imprimir.Enabled	= True
		
	END IF
	
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
	
END IF

IF il_NumFruta>0 AND dw_2.GetItemStatus(1, 0, Primary!) = NotModified! THEN
	il_NumFruta = 0
	il_NumEnva	= 0
END IF
end event

event resize;call super::resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

Tab_1.Width						=	This.WorkSpaceWidth() - 374
maximo 								=	tab_1.width

dw_2.x								=	37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y								=	37

Tab_1.x								=	37 + Round((maximo - tab_1.width) / 2, 0)
Tab_1.y								=	64 + dw_2.Height
Tab_1.Height						=	This.WorkSpaceHeight() - tab_1.y - 41

Tab_1.Tp_1.dw_detalle.x			= 27
Tab_1.Tp_1.dw_detalle.y			= 36
Tab_1.Tp_1.dw_detalle.Height		= Tab_1.height - 180
Tab_1.Tp_1.dw_detalle.Width		= Tab_1.width - 92

Tab_1.Tp_2.dw_envases.x			= 27
Tab_1.Tp_2.dw_envases.y			= 36
Tab_1.Tp_2.dw_envases.Height	= Tab_1.height - 180
Tab_1.Tp_2.dw_envases.Width	= Tab_1.width - 92


end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtofrutagranel_proceso
boolean visible = false
integer x = 3566
integer y = 280
integer width = 192
integer height = 128
boolean titlebar = false
string title = ""
string dataobject = "dw_mant_movtoenvaenca"
boolean hscrollbar = false
boolean vscrollbar = false
boolean livescroll = false
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtofrutagranel_proceso
integer x = 507
integer y = 32
integer width = 2729
integer height = 676
integer taborder = 10
string dataobject = "dw_mant_movtofrutagranel_proceso"
end type

event dw_2::itemchanged;String	ls_Nula
String	ls_Columna

ls_Columna	=	dwo.Name
SetNull(ls_Nula)

Choose Case ls_Columna
	Case "clie_codigo"
		If NoExisteCliente(Integer(data)) Then
			This.SetItem(row,"clie_codigo",Integer(ls_Nula))
			Return 1
		End If
		istr_mant.Argumento[16] = data

		dw_2.GetChild("espe_codigo", idwc_especie)
		idwc_especie.SetTransObject(sqlca)
		idwc_especie.Retrieve(integer(data))
		idwc_especie.InsertRow(0)
		
		dw_2.Getchild("prod_codigo", idwc_productor)
		idwc_productor.SetTransObject(sqlca)
		idwc_productor.Retrieve(integer(data))	
		idwc_productor.InsertRow(0)
		
	Case "mfge_numero"
		If NOT ExisteDespacho(gstr_ParamPlanta.CodigoPlanta, Integer(istr_mant.argumento[2]), Integer(data)) Then
			This.SetItem(1,"mfge_numero", Long(ls_Nula))
			This.SetFocus()
			Return 1
		Else
			Tab_1.tp_1.Enabled	=	True
			If (istr_mant.argumento[2]='21') and  (istr_mant.argumento[7]<>'1') Then
   			Tab_1.tp_2.Enabled	=	True
	   	End If
			Return 0
		End If

	Case "plde_coorde"
		If Not iuo_Packing.Existe(Integer(Data), True, sqlca) Then
			This.SetItem(1, ls_Columna, Integer(ls_Nula))
			Return 1
		Else
			istr_mant.argumento[15]	=	Data
		End If
	
	Case "defg_docrel"
		If Not Existedocproceso(gstr_ParamPlanta.CodigoPlanta,Integer(Data)) Then
			This.SetItem(1, ls_Columna, integer(ls_Nula))
			Return 1
		Else
			HabilitaEncab(False)
		End If
	
	Case "orpr_tipord"
		If Integer(data) = 4 OR Integer(data) = 8 OR Integer(data) = 5 Then
			ii_Orpr_TipOrd = Integer(data)
			istr_Mant.Argumento[4]				=	data
			If ii_Orpr_TipOrd = 8 Then
				istr_Mant.Argumento[2]			=	"20"
			ElseIf ii_Orpr_TipOrd = 5 Then
				istr_Mant.Argumento[2]			=	"25"
			Else
				istr_Mant.Argumento[2]			=	"21"
			End If
			This.Object.tpmv_codigo[row] 	= 	Integer(istr_Mant.Argumento[2])
			This.Object.defg_docrel[row] 	= 	integer(ls_Nula)
		Else
			ii_Orpr_TipOrd = integer(ls_Nula)
			This.SetItem(1, ls_Columna, integer(ls_Nula))
			This.SetItem(1, "defg_docrel", integer(ls_Nula))
			Return 1
		End If
		
End Choose
HabilitaIngreso(ls_Columna)
end event

event dw_2::doubleclicked;//
end event

event dw_2::buttonclicked;call super::buttonclicked;Str_Busqueda	lstr_busq
String ls_nula

SetNull(ls_nula)

CHOOSE CASE dwo.Name
	CASE "b_bucaordenproceso"
		IF This.Object.defg_docrel.Protect = "0" THEN
			lstr_busq.argum[1]	=	istr_mant.argumento[1]
			lstr_Busq.Argum[15]	=	istr_mant.argumento[4]
			lstr_busq.argum[16]	=	istr_mant.Argumento[16]
			
			OpenWithParm(w_busc_spro_ordenproceso_traspaso, lstr_busq)
			lstr_busq	=	Message.PowerObjectParm

			IF lstr_busq.argum[1] <> "" and lstr_busq.argum[1]<> "0" THEN
				dw_4.reset()
				dw_5.reset()
				IF Not Existedocproceso(gstr_ParamPlanta.CodigoPlanta,Integer(lstr_busq.argum[1])) THEN
					dw_2.SetItem(1, 'defg_docrel', integer(ls_Nula))
					RETURN 1
				ELSE
					dw_2.Setitem(1, "defg_docrel", Integer(istr_mant.argumento[5]))
				END IF
		
				HabilitaIngreso('defg_docrel')
				HabilitaEncab(False)
			END IF
		END IF
END CHOOSE		
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtofrutagranel_proceso
integer x = 3918
integer y = 204
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;HabilitaEncab(TRUE)
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtofrutagranel_proceso
integer x = 3918
integer y = 384
integer taborder = 70
end type

event pb_eliminar::clicked;str_Mant		lstr_Mant

lstr_Mant.Argumento[1]	=	"1"
lstr_Mant.Argumento[2]	=	istr_Mant.Argumento[1]
lstr_Mant.Argumento[3]	=	""
lstr_Mant.Argumento[4]	=	"E"
lstr_Mant.Argumento[5]	=	string(dw_4.Object.lote_pltcod[1])
lstr_Mant.Argumento[6]	=	String(dw_4.Object.lote_espcod[1])
lstr_Mant.Argumento[7]	=	String(dw_4.Object.lote_codigo[1])

Parent.TriggerEvent("ue_borrar")

OpenWithParm(w_mant_bitacorasitua, lstr_Mant)




end event

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtofrutagranel_proceso
integer x = 3918
integer y = 564
integer taborder = 80
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtofrutagranel_proceso
integer x = 3918
integer y = 744
integer taborder = 90
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtofrutagranel_proceso
integer x = 3918
integer y = 924
integer taborder = 100
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtofrutagranel_proceso
integer x = 3918
integer y = 1436
integer taborder = 30
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtofrutagranel_proceso
integer x = 3918
integer y = 1612
integer taborder = 40
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtofrutagranel_proceso
integer x = 3918
integer y = 24
integer taborder = 50
end type

type tab_1 from tab within w_maed_movtofrutagranel_proceso
event create ( )
event destroy ( )
integer x = 46
integer y = 744
integer width = 3840
integer height = 1092
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean fixedwidth = true
boolean raggedright = true
boolean focusonbuttondown = true
integer selectedtab = 1
tp_1 tp_1
tp_2 tp_2
end type

on tab_1.create
this.tp_1=create tp_1
this.tp_2=create tp_2
this.Control[]={this.tp_1,&
this.tp_2}
end on

on tab_1.destroy
destroy(this.tp_1)
destroy(this.tp_2)
end on

event selectionchanged;IF NewIndex = 1 THEN
	IF dw_4.RowCount() > 0 THEN
		il_Fila 					=	1
		pb_ins_det.Enabled	=	NOT istr_Mant.Solo_Consulta
		pb_eli_det.Enabled	=	NOT istr_Mant.Solo_Consulta
	ELSE
		pb_eli_det.Enabled	=	False
	END IF
ELSE
	IF dw_5.RowCount() > 0 THEN
		pb_ins_det.Enabled	=	NOT istr_Mant.Solo_Consulta
		pb_eli_det.Enabled	=	NOT istr_Mant.Solo_Consulta
		il_Fila 					=	1
	ELSE
		pb_eli_det.Enabled	=	False
	END IF
END IF
end event

type tp_1 from userobject within tab_1
event create ( )
event destroy ( )
integer x = 18
integer y = 116
integer width = 3803
integer height = 960
boolean enabled = false
long backcolor = 16777215
string text = "Detalle de Fruta"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Table!"
long picturemaskcolor = 553648127
dw_detalle dw_detalle
end type

on tp_1.create
this.dw_detalle=create dw_detalle
this.Control[]={this.dw_detalle}
end on

on tp_1.destroy
destroy(this.dw_detalle)
end on

type dw_detalle from uo_dw within tp_1
integer x = 27
integer y = 24
integer width = 3749
integer height = 916
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mant_mues_detamovto_proceso"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event doubleclicked;//
end event

event type long dwnkey(keycode key, unsignedlong keyflags);il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutagranel_proceso.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_proceso.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event getfocus;//IF il_fila > 0 THEN This.SelectRow(il_fila, True)
//
//RETURN 0
end event

event itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event rowfocuschanged;//ib_datos_ok = True
//
//IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
//	ib_datos_ok = False
//ELSE
//	il_fila = getrow()
//	This.SelectRow(0,False)
//	This.SelectRow(il_fila,True)
//END IF
//
//RETURN 0
end event

event type long ue_seteafila(unsignedlong wparam, long lparam);il_fila	= This.GetRow()

This.SelectRow(0, False)
This.SelectRow(il_fila, True)

RETURN 0
end event

event itemchanged;call super::itemchanged;Integer	li_Planta, li_Especie, li_Numero, li_TipoEnvase, li_Envase, li_Cliente
String	ls_Columna, ls_Nula
Long     ll_bultos

li_Cliente	=	Integer(istr_mant.argumento[16])

ls_Columna 	= 	dwo.Name

SetNull(ls_Nula)

CHOOSE CASE ls_Columna
	CASE "enva_tipoen"
     IF Duplicado('enva_tipoen',data) THEN
			This.Setitem(row,'enva_tipoen',integer(ls_nula))
			RETURN 1
	  ELSE	
			This.GetChild("enva_codigo", idwc_Envases)
			idwc_Envases.SetTransObject(sqlca)
			IF idwc_Envases.Retrieve(integer(data)) = 0 THEN
				idwc_Envases.InsertRow(0)
			END IF
	  END IF
	  
	 CASE "enva_codigo"
		  li_TipoEnvase	=	This.Object.enva_tipoen[row]
		  IF NOT iuo_envases.Existe(li_TipoEnvase,Integer(data), True, Sqlca) OR &
				 Duplicado('enva_codigo',data) THEN
				This.Setitem(row,'enva_codigo',integer(ls_nula))
				This.Setitem(row,'enva_nombre',ls_nula)
				RETURN 1
		  ELSE	
			  This.Setitem(row,'enva_nombre',iuo_envases.Nombre)
		  END IF
	  
	 CASE "lotes"
		IF Len(data) = 10 THEN
			li_Planta	=	Integer(Mid(data,1,4))
			li_Especie	=	Integer(Mid(data,5,2))
			li_Numero	=	Integer(Mid(data,7,4))
		
			IF NOT iuo_Lotes.Existe(li_Planta, li_Especie, li_Numero, True, SQLCA) OR &
			    Duplicado('lotes',data) THEN
				This.Setitem(row,'lotes',ls_nula)
				RETURN 1
			ELSE
				This.Setitem(row,'lote_pltcod',li_Planta)	
				This.Setitem(row,'lote_espcod',li_Especie)
				This.Setitem(row,'lote_codigo',li_Numero)
				
				is_lote=data
			END IF
		ELSE
			This.SetItem(row,'lotes',ls_nula)
			RETURN 1
		END IF
	 
	 CASE "lote_codigo"
			li_Planta	=	This.Object.lote_pltcod[row]
			IF isnull(li_planta) THEN
				MessageBox("Atención","Ingrese primero una planta para el lote.")
				This.Setitem(row,'lotes',ls_nula)
				This.Setitem(row,'lote_codigo',integer(ls_nula))
				RETURN 1
			END IF
			li_Especie	=	This.Object.lote_espcod[row]
			li_Numero	=	Integer(data)
	      is_lote=string(li_planta,'0000')+string(li_especie,'00')+string(li_numero,'0000')
			
			IF NOT iuo_Lotes.Existe(li_Planta, li_Especie, li_Numero, True, SQLCA, ii_Orpr_TipOrd) OR &
			    Duplicado('lotes',is_lote) THEN
				This.Setitem(row,'lotes',ls_nula)
				This.Setitem(row,'lote_codigo',integer(ls_nula))
				is_lote=""
				RETURN 1
			ELSE
				IF iuo_lotes.Productor <> dw_2.Object.prod_codigo[1] THEN
					Messagebox("Error de Consistencia","El lote ingresado pertenece a otro Productor.")
					This.Setitem(row,'lotes',ls_nula)
					This.Setitem(row,'lote_codigo',integer(ls_nula))
 					is_lote=""
					RETURN 1
				END IF	
				This.SetItem(row,"lotes",is_lote)
			END IF
			
	 CASE "cama_codigo" 
		
		IF NOT iuo_Camara.Existe(Integer(istr_Mant.Argumento[1]),Integer(data),True,SQLCA) OR &
		    Duplicado('cama_codigo',data) THEN
			This.SetItem(row, ls_columna,Integer(ls_Nula))
			This.SetItem(row, "mfgd_bulent",Long(ls_Nula))
			This.SetItem(row, "bultosdesp", Long(ls_Nula))
			RETURN 1
		ELSE
			li_TipoEnvase	=	This.Object.enva_tipoen[row]
			li_Envase		=	This.Object.enva_codigo[row]
			
			IF iuo_Lotes.ObtieneSaldoCamara(Integer(istr_Mant.Argumento[1]),Integer(data),li_TipoEnvase,li_Envase,&
														ll_Bultos, True, SQLCA) THEN
				This.Setitem(row,'mfgd_bulent',ll_bultos)
				This.Setitem(il_fila,'bultosdesp',ll_bultos)			
			ELSE
				This.SetItem(row,ls_Columna,Integer(ls_nula))
				RETURN 1
			END IF
		END IF
END CHOOSE
end event

event buttonclicked;call super::buttonclicked;If Not istr_Mant.Solo_Consulta  Then
	Choose Case dwo.Name
		Case "b_detallelotes"
			 w_maed_movtofrutagranel_proceso.TriggerEvent("ue_modifica_detalle")
			
		Case "buscaenvase"
			BuscaEnvase()
	End Choose
End If
end event

type tp_2 from userobject within tab_1
event create ( )
event destroy ( )
integer x = 18
integer y = 116
integer width = 3803
integer height = 960
boolean enabled = false
long backcolor = 16777215
string text = "Detalle de Envases       "
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "ArrangeTables!"
long picturemaskcolor = 553648127
dw_envases dw_envases
end type

on tp_2.create
this.dw_envases=create dw_envases
this.Control[]={this.dw_envases}
end on

on tp_2.destroy
destroy(this.dw_envases)
end on

type dw_envases from uo_dw within tp_2
integer x = 27
integer y = 24
integer width = 3749
integer height = 916
integer taborder = 10
string dataobject = "dw_mues_movtoenvadeta"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event clicked;IF Row > 0 THEN
	il_fila = Row
//	//This.SelectRow(0,False)
//	This.SetRow(il_fila)
//	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event doubleclicked;w_maed_movtofrutagranel_proceso.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

event type long dwnkey(keycode key, unsignedlong keyflags);il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutagranel_proceso.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutagranel_proceso.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event getfocus;//IF il_fila > 0 THEN This.SelectRow(il_fila, True)
//
//RETURN 0
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event rowfocuschanged;ib_datos_ok = True

IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = getrow()
//	This.SelectRow(0,False)
//	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event type long ue_seteafila(unsignedlong wparam, long lparam);il_fila	= This.GetRow()

//This.SelectRow(0, False)
//This.SelectRow(il_fila, True)

RETURN 0
end event

type dw_3 from datawindow within w_maed_movtofrutagranel_proceso
boolean visible = false
integer x = 3346
integer y = 104
integer width = 192
integer height = 128
integer taborder = 120
boolean bringtotop = true
string dataobject = "dw_mant_spro_lotes_clasificados"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

type dw_10 from datawindow within w_maed_movtofrutagranel_proceso
boolean visible = false
integer x = 3346
integer y = 268
integer width = 192
integer height = 128
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_movtofrutagranel_proceso"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_lotpro from datawindow within w_maed_movtofrutagranel_proceso
boolean visible = false
integer x = 3529
integer y = 84
integer width = 192
integer height = 128
integer taborder = 90
boolean bringtotop = true
string title = "Solo para los procesos automaticos."
string dataobject = "dw_mues_spro_lotesfrutagradet_recepcion"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

