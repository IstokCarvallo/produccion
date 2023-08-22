$PBExportHeader$w_info_etiqetas_compactos.srw
$PBExportComments$Ventana de mantención de Orden de Proceso de Fruta Granel.
forward
global type w_info_etiqetas_compactos from w_mant_encab_deta
end type
type dw_7 from datawindow within w_info_etiqetas_compactos
end type
type dw_crea_caja from datawindow within w_info_etiqetas_compactos
end type
type tab_1 from tab within w_info_etiqetas_compactos
end type
type tp_1 from userobject within tab_1
end type
type dw_3 from datawindow within tp_1
end type
type tp_1 from userobject within tab_1
dw_3 dw_3
end type
type tp_2 from userobject within tab_1
end type
type dw_4 from uo_dw within tp_2
end type
type tp_2 from userobject within tab_1
dw_4 dw_4
end type
type tp_3 from userobject within tab_1
end type
type dw_5 from uo_dw within tp_3
end type
type tp_3 from userobject within tab_1
dw_5 dw_5
end type
type tp_4 from userobject within tab_1
end type
type dw_6 from uo_dw within tp_4
end type
type tp_4 from userobject within tab_1
dw_6 dw_6
end type
type tab_1 from tab within w_info_etiqetas_compactos
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
tp_4 tp_4
end type
type hpb_impresion from hprogressbar within w_info_etiqetas_compactos
end type
type gb_3 from groupbox within w_info_etiqetas_compactos
end type
end forward

global type w_info_etiqetas_compactos from w_mant_encab_deta
integer width = 4119
integer height = 2116
string title = "IMPRESION DE ADHESIVOS  EMBALAJE"
string menuname = ""
boolean resizable = false
dw_7 dw_7
dw_crea_caja dw_crea_caja
tab_1 tab_1
hpb_impresion hpb_impresion
gb_3 gb_3
end type
global w_info_etiqetas_compactos w_info_etiqetas_compactos

type variables
uo_especie 			 		iuo_especie
uo_variedades 		 		iuo_variedad
uo_grupoespecie    		iuo_grupo
uo_subgrupoespecie 		iuo_subgrupo
uo_spro_ordenproceso 	iuo_ordenproceso
uo_productores				iuo_productores
uo_tratamientofrio			iuo_tratamientofrio
uo_periodofrio				iuo_periodofrio
uo_lineapacking			iuo_lineapacking
uo_lotesfrutagranel		iuo_lotesfrutagranel
uo_Paramprecos			iuo_Paramprecos
uo_productores    			iuo_productor
uo_predios        			iuo_predio
uo_embalajesprod 		iuo_embalajesprod
uo_buscadatosproceso	iuo_buscadatosproceso
uo_lotescorrelequipo_gr	iuo_correl
uo_variedades				iuo_variedades
uo_entidades				iuo_entidad
uo_etiquetas				iuo_etiq
uo_cliente					iuo_clie
uo_voicecode				iuo_voicecode

DataWindowChild  		idwc_pltadesp, idwc_tipdoc, idwc_numero, idwc_planta,&
                  	idwc_especie, idwc_variedad, idwc_serplanta, 	idwc_linea, idwc_predio, idwc_procprog, idwc_varrot

//DataWindow			dw_8, dw_9, dw_10

Date 						idt_fechaIni, idt_fechater

Integer					ii_lote, ii_proceso, ii_varrot
String					is_embalaje, is_calibre,  is_Computador
Integer					ii_zona, ii_Cliente, ii_Planta, ii_categoria, ii_etiqueta, ii_etiquetas
Date						id_Fecha
Boolean					ib_FlagRetrieve
end variables

forward prototypes
public function boolean noexistecliente (integer cliente)
public function boolean existeprograma (string as_columna, string as_valor)
public function boolean existeprocesos (date adt_fechaproc)
public subroutine imprime_dw (ref datawindow al_dwimprime)
public function boolean despieceregistro (string as_registro)
public function boolean buscanuevocorrelativo (integer ai_bodega, integer ai_cliente)
public subroutine imprime_compacto ()
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine ciclo_compacto ()
public function boolean buscadatosproceso (long al_proceso, integer ai_tipord, integer ai_fila)
public subroutine cargamaximo ()
end prototypes

public function boolean noexistecliente (integer cliente);String ls_nombre

SELECT 	clie_nombre
INTO		:ls_nombre
FROM 		dba.clientesprod
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

public function boolean existeprograma (string as_columna, string as_valor);Long		ll_Numero
String	ls_Nombre
Boolean	lb_Retorno = True
Date		ldt_fecha
Integer	li_planta, li_especie, li_cliente

li_Planta		=	dw_2.Object.plde_codigo[1]
li_Especie	=	dw_2.Object.espe_codigo[1]
ll_Numero	=	dw_2.Object.ppre_numero[1]
li_Cliente		=	dw_2.Object.clie_codigo[1]

//IF li_Cliente <> 81 THEN
//	dw_2.Object.Camara.Visible	=	True
//	dw_2.Object.Camara.Visible	=	True
//	dw_2.Object.Camara.Enabled	=	True
//ELSE
//	dw_2.Object.Camara.Visible	=	False
//	dw_2.Object.Camara.Visible	=	False
//	dw_2.Object.Camara.Enabled	=	False
//END IF

CHOOSE CASE as_columna
	CASE "plde_codigo"
		li_Planta	=	Integer(as_Valor)
		
	CASE "espe_codigo"
		li_Especie	=	Integer(as_Valor)
		
	CASE "ppre_numero"
		ll_Numero	=	Long(as_Valor)
		
END CHOOSE

IF Isnull(li_Planta) OR Isnull(li_Especie) OR IsNull(ll_Numero) THEN
	lb_Retorno	=	True
ELSE
	SELECT	ppre_nombre, ppre_feccre
		INTO	:ls_Nombre, :ldt_fecha
		FROM	dba.spro_programaprocenca
		WHERE	plde_codigo	=	:li_Planta
		AND	espe_codigo		=	:li_Especie
		AND	ppre_numero	=	:ll_Numero
		AND   clie_codigo 		=  :li_Cliente;
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Programa de Procesos")
		
		lb_Retorno	=	False
	ELSEIF SQLCA.SQLCode = 100 THEN
		MessageBox("Atención","Programa no ha sido registrado")
		lb_Retorno	=	False
	ELSE
		dw_2.Object.ppre_nombre[1]	=	ls_Nombre

		PostEvent("ue_recuperadatos")
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean existeprocesos (date adt_fechaproc);Integer	li_Planta, li_Especie, li_Cantidad, li_Cliente
Long		ll_Numero
Boolean	lb_Retorno = True

li_Planta		=	dw_2.Object.plde_codigo[1]
li_Especie	=	dw_2.Object.espe_codigo[1]
ll_Numero	=	dw_2.Object.ppre_numero[1]
li_Cliente		=	dw_2.Object.clie_codigo[1]

SELECT	Count(sp.pprd_secuen)
	INTO	:li_Cantidad
	FROM	dba.spro_progordenproceso as sp, 
			dba.spro_programaprocenca as se
	WHERE	sp.plde_codigo	=	:li_Planta
	AND	sp.espe_codigo		=	:li_Especie
	AND	sp.ppre_numero	=	:ll_Numero
	AND   se.clie_codigo 		=  :li_Cliente	
	AND	sp.popr_fecpro		=	:adt_FechaProc
	AND   sp.plde_codigo		=	se.plde_codigo
	AND	sp.espe_codigo		=	se.espe_codigo
	AND	sp.ppre_numero	=  se.ppre_numero
	AND	se.clie_codigo		=	:li_Cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Programa de Ordenes Procesos")
	
	lb_Retorno	=	False
ELSEIF SQLCA.SQLCode = 100 THEN
	lb_Retorno	=	False
ELSE
	IF isnull(li_Cantidad) or li_cantidad=0 THEN lb_Retorno	=	False 	
END IF

RETURN lb_Retorno
end function

public subroutine imprime_dw (ref datawindow al_dwimprime);SetPointer(HourGlass!)
Integer	li_repite
Long		ll_filas, ll_trabajo, ll_cajas, ll_etiquetas, ll_proceso
String		ls_texto

dw_7.DataObject = "dw_info_adhesivo_1"
dw_7.SetTransObject(Sqlca)

FOR ll_filas = 1 to al_dwimprime.RowCount()
	ll_proceso	=	al_dwimprime.Object.orden[ll_filas]
	ls_texto		=	al_dwimprime.Object.tipo[ll_filas]
	ls_texto		=	String( ( tab_1.SelectedTab ) , "00" ) + ls_texto
	ll_cajas		=	al_dwimprime.Object.cajas[ll_filas]
	FOR ll_etiquetas = 1 TO ll_cajas
		
	NEXT
NEXT

SetPointer(Arrow!)
end subroutine

public function boolean despieceregistro (string as_registro);Integer		li_lectura, li_sigte=1, li_larg, li_lectura2, li_exis1 = 0, li_exis2 = 0, li_exis3 = 0 ,li_exis4 = 0
String		ls_caracter, ls_compone, ls_compone2, ls_mensaje = "", ls_Null

SetNull(ls_Null)

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
				dw_1.Object.capr_calibr[1]	=	is_calibre
					
		END CHOOSE
		dw_1.Object.capr_estado[1]   = 0			
		li_lectura = li_sigte - 1
		
	END IF
NEXT

IF li_exis1 = 0 then
	ls_mensaje = ls_mensaje + "~nProceso "
END IF

IF li_exis2 = 0 then
	ls_mensaje = ls_mensaje + "~nLote "
END IF

IF li_exis3 = 0 then
	ls_mensaje = ls_mensaje + "~nEmbalaje "
END IF

IF li_exis4 = 0 then
	ls_mensaje = ls_mensaje + "~nCalibre "
END IF

IF ls_mensaje<>"" THEN
//	MessageBox("Error de Lectura", "No se pudo leer :" + &
//						ls_mensaje + ".", StopSign!, Ok!)
	RETURN FALSE
ELSE
	RETURN TRUE
END IF
end function

public function boolean buscanuevocorrelativo (integer ai_bodega, integer ai_cliente);Long	ll_Numero
SetPointer(HourGlass!)

IF NOT iuo_correl.ExisteCorrel(Integer(istr_mant.argumento[1]),99, &
								is_Computador, TRUE, sqlca) THEN						
	RETURN FALSE
ELSE
	dw_1.Object.capr_numero[1]			=	iuo_correl.il_correcompa
	dw_1.Object.capr_pcline[1]			=	is_Computador
	dw_1.Object.capr_lineas[1]			=	iuo_correl.loco_comlin
	dw_7.DataObject						=	iuo_correl.loco_dwcomp
	dw_7.SetTransObject(SQLCA)
	
	RETURN True
END IF


end function

public subroutine imprime_compacto ();Long		ll_Fila, ll_recorre, ll_inicial, ll_final, li_resta=0
String	ls_codigo, ls_fecha

SetPointer(HourGlass!)

//IF dw_2.Object.espe_codigo[1] = 21 THEN  li_resta = 1

		
FOR ll_recorre = 1 TO dw_1.RowCount() //STEP iuo_correl.foad_canocx
	ll_inicial	=	ll_recorre
	ll_final		=	iuo_correl.foad_canocx + (ll_recorre - 1 -li_resta)

	IF ll_final > dw_1.RowCount() THEN
		ll_final = dw_1.RowCount()
	END IF
	
	iuo_clie.Existe(dw_2.Object.clie_codigo[1], False, sqlca)
	
	IF dw_2.Object.clie_codigo[1] <> 81 AND dw_2.Object.clie_codigo[1] <> 590 AND dw_2.Object.clie_codigo[1] <> 15 THEN

		ll_Fila = dw_7.Retrieve(dw_1.Object.clie_codigo[1], &
										dw_1.Object.plde_codigo[1], &
								 Long(dw_1.Object.capr_numero[ll_inicial]),& 
								 Long(dw_1.Object.capr_numero[ll_final]),&
										1,									 &
										dw_2.Object.Camara[1],		 &
							 Integer(dw_2.Object.formato[1]))
							 
		IF iuo_correl.foad_canocx = 1 THEN
			FOR ll_Fila = 1 TO dw_7.RowCount()
				dw_7.Object.envo_descrip.visible		=	1
			NEXT
		END IF
	
	ELSE
		ll_Fila = dw_7.Retrieve(dw_1.Object.clie_codigo[1], &
										dw_1.Object.plde_codigo[1], &
								 Long(dw_1.Object.capr_numero[ll_inicial]),&
								 Long(dw_1.Object.capr_numero[ll_final]),	 1)	
	END IF
	
	IF ll_Fila > 0 AND ii_etiquetas > 0 THEN
		IF  ii_etiquetas > 0 THEN
			IF dw_2.Object.clie_codigo[1] <> 81 AND dw_2.Object.clie_codigo[1] <> 590 AND dw_2.Object.clie_codigo[1] <> 15  THEN
					
					ls_codigo								=	'00' + String(dw_7.Object.zona_codigo[1], '00') 
					ls_codigo			=	ls_codigo	+	String(dw_7.Object.plde_codigo[1]		 , '0000')
					ls_codigo			=	ls_codigo	+	String(dw_7.Object.capr_numero[1] 		 , '0000000000')
				
			ELSE
					ls_fecha			=	String(dw_7.Object.capr_fecemb[1])
					ls_fecha			=	Left(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Right(ls_fecha, 2)
		
					ls_codigo			=	"01" + dw_7.Object.emba_nroint[1] + "10" + ls_fecha + "\F"
					ls_codigo			=	ls_codigo	+	"21" + String(dw_7.Object.plde_codigo[1], "0000") 
					ls_codigo			=	ls_codigo	+	String(dw_7.Object.capr_numero[1], '00000000')		
					
					IF dw_2.Object.clie_codigo[1] = 590 THEN
						IF dw_2.Object.formato[1] = '0' THEN	
							//dw_7.Modify("compute_3.Text = '" + dw_7.Object.prpr_nombre[1] +  " SDP: " + dw_7.Object.prpr_nombre[1] + "'")
							dw_7.Object.calcod[1] = dw_2.Object.camara[1]//dw_7.Object.prpr_nombre[1] +  " SDP: " + dw_7.Object.prpr_nombre[1]
						ELSE
							//dw_7.Modify("compute_3.Text = '" + dw_7.Object.prpr_nombre[1] +   ' COD. SAG: ' +  dw_7.Object.nume_intern[1] + ' ' + dw_2.Object.camara[1] + "'")
							dw_7.Object.calcod[1] =	dw_2.Object.camara[1]//dw_7.Object.prpr_nombre[1] + ' COD. SAG: ' +  dw_7.Object.nume_intern[1] + ' ' +
						END IF
					END IF
			END IF
			
			dw_7.Object.Ole_1.Object.BarCode		=	iuo_clie.Barras
			dw_7.Object.Ole_1.Object.Text 			= 	ls_codigo
			/*
				Code Pick Voice
			 */
			iuo_voicecode	=	Create uo_voicecode
			iuo_voicecode.voicecode(dw_7	, dw_7.Object.emba_nroint[1],  ls_fecha, dw_7.Object.capr_fecemb[1], iuo_correl.foad_vopico)
			
			ii_etiquetas										=	ii_etiquetas - 1
		ELSE
			dw_7.Object.Ole_1.Object.BarCode		=	iuo_clie.Barras
			dw_7.Object.Ole_1.Object.Text 		= 	""			
		END IF
		
		IF (iuo_correl.foad_canocx - li_resta)  = 2 AND dw_7.RowCount() > 1 THEN
			IF  ii_etiquetas > 0 THEN
				IF dw_2.Object.clie_codigo[1] <> 81  AND dw_2.Object.clie_codigo[1] <> 590 AND dw_2.Object.clie_codigo[1] <> 15 THEN
					
					ls_codigo								=	'00' + String(dw_7.Object.zona_codigo[2], '00') 
					ls_codigo			=	ls_codigo	+	String(dw_7.Object.plde_codigo[2]		 , '0000')
					ls_codigo			=	ls_codigo	+	String(dw_7.Object.capr_numero[2] 		 , '0000000000')
					
				ELSE
					ls_fecha			=	String(dw_7.Object.capr_fecemb[2])
					ls_fecha			=	Left(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Right(ls_fecha, 2) 
			
					ls_codigo			=	"01" + dw_7.Object.emba_nroint[2] + "10" + ls_fecha + "\F"
					ls_codigo			=	ls_codigo	+	"21" + String(dw_7.Object.plde_codigo[2], "0000") 
					ls_codigo			=	ls_codigo	+	String(dw_7.Object.capr_numero[2], '00000000')
					
					IF dw_2.Object.clie_codigo[1] = 590 THEN
						IF dw_2.Object.formato[1] = '0' THEN								
							dw_7.Object.calcod[1] = dw_2.Object.camara[1]
						ELSE
							dw_7.Object.calcod[1] = dw_2.Object.camara[1]
						END IF
					END IF
					
				END IF
				dw_7.Object.Ole_2.Object.BarCode	=	iuo_clie.Barras
				dw_7.Object.Ole_2.Object.Text 		= 	ls_codigo
				
				/*
					Code Pick Voice
				 */
				iuo_voicecode	=	Create uo_voicecode
				iuo_voicecode.voicecode(dw_1	, dw_1.Object.emba_nroint[1],  ls_fecha, dw_1.Object.capr_fecemb[1], iuo_correl.foad_vopico)
			
				ii_etiquetas									=	ii_etiquetas - 1
			ELSE
				dw_7.Object.Ole_2.Object.BarCode	=	iuo_clie.Barras
				dw_7.Object.Ole_2.Object.Text 		= 	""
				
			END IF			
		END IF
		
		IF (iuo_correl.foad_canocx - li_resta) = 3 AND dw_7.RowCount() > 2 THEN
			IF  ii_etiquetas > 0 THEN
				IF dw_2.Object.clie_codigo[1] <> 81 AND dw_2.Object.clie_codigo[1] <> 590 AND dw_2.Object.clie_codigo[1] <> 15 THEN
					
					ls_codigo			=	'00' + String(dw_7.Object.zona_codigo[3], '00') 
					ls_codigo			=	ls_codigo	+	String(dw_7.Object.plde_codigo[3]		 , '0000')
					ls_codigo			=	ls_codigo	+	String(dw_7.Object.capr_numero[3] 		 , '0000000000')
					
				ELSE
					ls_fecha			=	String(dw_7.Object.capr_fecemb[3])
					ls_fecha			=	Left(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Right(ls_fecha, 2) 
			
					ls_codigo			=	"01" + dw_7.Object.emba_nroint[3] + "10" + ls_fecha + "\F"
					ls_codigo			=	ls_codigo	+	"21" + String(dw_7.Object.plde_codigo[3], "0000") 
					ls_codigo			=	ls_codigo	+	String(dw_7.Object.capr_numero[3], '00000000')
					
					IF dw_2.Object.clie_codigo[1] = 590 THEN
						IF dw_2.Object.formato[1] = '0' THEN								
							dw_7.Object.calcod[1] = dw_2.Object.camara[1]
						ELSE
							dw_7.Object.calcod[1] = dw_2.Object.camara[1]
						END IF
					END IF
					
				END IF
				dw_7.Object.Ole_3.Object.BarCode	=	iuo_clie.Barras
				dw_7.Object.Ole_3.Object.Text 		= 	ls_codigo
				/*
					Code Pick Voice
				 */
				iuo_voicecode	=	Create uo_voicecode
				iuo_voicecode.voicecode(dw_7	, dw_7.Object.emba_nroint[1],  ls_fecha, dw_7.Object.capr_fecemb[1], iuo_correl.foad_vopico)				
				ii_etiquetas									=	ii_etiquetas - 1
			ELSE
				dw_7.Object.Ole_3.Object.BarCode	=	iuo_clie.Barras
				dw_7.Object.Ole_3.Object.Text 		= 	""
				
			END IF
			
		END IF
	ELSE
		MessageBox("Error Compactos", "No es posible recuperar el compacto para realizar su impresion", StopSign!)
		Return
	
	END IF
	dw_7.Print(False, False)
	
	ll_recorre	=	iuo_correl.foad_canocx + (ll_recorre - 1 - li_resta)
	
NEXT

SetPointer(Arrow!)
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);IF dw_crea_caja.Object.clie_codigo[1] <> -1 THEN 
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

public subroutine ciclo_compacto ();Integer 	li_filas, 		li_etiquetas, 	li_compactos,	li_TipOrd, li_contratista, li_tipo, li_varrot, li_fila
Long		ll_proceso, 	ll_lote, 		ll_embaladora, ll_embala
String	ls_embalaje, 	ls_calibre, 	ls_Registro, 	ls_formato, ls_calrot

IF NOT iuo_correl.ExisteCorrel(Integer(istr_mant.argumento[1]),99, &
								is_Computador, TRUE, sqlca) THEN						
	RETURN 
END IF

CargaMaximo()

FOR li_filas = 1 TO tab_1.tp_1.dw_3.RowCount()	
	IF tab_1.tp_1.dw_3.Object.cajas[li_filas] > 0 THEN		
		ll_proceso						=	tab_1.tp_1.dw_3.Object.orpr_numero[li_filas]
		li_tipo								=	tab_1.tp_1.dw_3.Object.orpr_tipord[li_filas]
		ll_lote								=	Long(tab_1.tp_1.dw_3.Object.lote_codigo[li_filas])
		ls_embalaje						=	tab_1.tp_1.dw_3.Object.emba_codigo[li_filas]
		ls_calibre						=	tab_1.tp_1.dw_3.Object.caen_calibr[li_filas]
		ls_calrot							=	tab_1.tp_1.dw_3.Object.caen_calrot[li_filas]
		ii_categoria						=	tab_1.tp_1.dw_3.Object.cate_codigo[li_filas]
		ii_etiqueta						=	tab_1.tp_1.dw_3.Object.etiq_codigo[li_filas]	
		li_etiquetas						=	tab_1.tp_1.dw_3.Object.cajas[li_filas]
		li_varrot							=	tab_1.tp_1.dw_3.Object.capr_varrot[li_filas]
		ll_embala						=	tab_1.tp_1.dw_3.Object.capr_embala[li_filas]
		
		dw_crea_caja.Reset()
		For li_compactos = 1 To li_etiquetas
			dw_7.Reset()
			li_fila	=	dw_crea_caja.Retrieve(dw_2.Object.clie_codigo[1], &
														 dw_2.Object.plde_codigo[1], &
														 li_tipo, &
														 ll_proceso, &
														 li_varrot, &
														 ls_embalaje, &
														 ls_calibre, &
														 ii_etiqueta, &
														 ll_embala, &
														 gstr_us.computador, &
														 ii_categoria, &
														 ls_calrot)
			IF li_fila = 1 THEN
				IF dw_crea_caja.Object.clie_codigo[1] <> -1 THEN
					
					dw_7.DataObject						=	dw_crea_caja.Object.formato[1]
					
					dw_7.SetTransObject(SQLCA)
					dw_1.Retrieve(dw_2.Object.clie_codigo[1], &
									  dw_2.Object.plde_codigo[1], dw_crea_caja.Object.nrocaja[1])
					
					Commit;
					IF sqlca.sqlcode <> 0 THEN
						MessageBox("Error", "No se puede Generar Compacto. ~r~n" + String(dw_crea_caja.Object.formato[1]))
						This.TriggerEvent("ue_imprimirerror")
						Exit
					ELSE
						hpb_impresion.StepIt()
					END IF
									  
				ELSE
					MessageBox("Error", "No se puede Generar Compacto. ~r~n" + String(dw_crea_caja.Object.formato[1]))
					This.TriggerEvent("ue_imprimirerror")
					Exit
				End If
				
			ELSE
				MessageBox("Error", "No se puede Generar Compacto.")
				This.TriggerEvent("ue_imprimirerror")
				Exit
				
			End If
		Next
		
		IF dw_1.RowCount() > 0 THEN
			dw_1.SetSort("capr_numero asc")
			dw_1.Sort()
			ii_etiquetas	=	li_etiquetas
			Imprime_compacto()
			dw_1.Reset()
		END IF
	END IF
	
	tab_1.tp_1.dw_3.Object.cajas[li_filas] = 	0
	ib_FlagRetrieve								=	False
	
NEXT

hpb_impresion.Visible						=	False
end subroutine

public function boolean buscadatosproceso (long al_proceso, integer ai_tipord, integer ai_fila);Integer	li_Planta
String	ls_Null

SetNull(ls_Null)
		
iuo_buscadatosproceso	=	Create	uo_buscadatosproceso	
iuo_embalajesprod			=	Create	uo_embalajesprod	
iuo_productor				=	Create	uo_productores
iuo_variedad				=	Create	uo_variedades

IF iuo_buscadatosproceso.existe(dw_2.Object.plde_codigo[1], dw_2.Object.clie_codigo[1], al_proceso,ai_TipOrd,True,Sqlca)	THEN

	iuo_buscadatosproceso.buscapredio(ii_lote,False,Sqlca)
	
	iuo_buscadatosproceso.buscaplanta(iuo_buscadatosproceso.Planta,False,Sqlca)
	
	iuo_buscadatosproceso.buscacatetiq(al_proceso,False,Sqlca)
	
	dw_1.Object.clie_codigo[1]			=	iuo_buscadatosproceso.Cliente
	dw_1.Object.plde_codigo[1]			=	iuo_buscadatosproceso.Planta
	dw_1.Object.capr_fecemb[1] 		=	iuo_buscadatosproceso.Fecha
	dw_1.Object.frio_tipofr[1] 		=	iuo_buscadatosproceso.frio_tipofr
	
	dw_1.Object.capr_tipdoc[1]			=	ai_tipord
	
	li_Planta								=	iuo_buscadatosproceso.Planta
	
	iuo_ParamPrecos						=	Create uo_Paramprecos       
	
	IF  iuo_buscadatosproceso.Predio > 0 THEN
		dw_1.Object.prod_predio[1]   	=	iuo_buscadatosproceso.Predio
		dw_1.Object.prod_huerto[1]   	=	iuo_buscadatosproceso.Predio
	ELSE	
		dw_1.Object.prod_predio[1]  	=	Integer(ls_Null)
		dw_1.Object.prod_huerto[1]   	=	Integer(ls_Null)
	END IF
	 
	IF iuo_buscadatosproceso.Cuartel > 0 THEN
		dw_1.Object.prod_cuarte[1]   	=	iuo_buscadatosproceso.Cuartel
	ELSE
		dw_1.Object.prod_cuarte[1]   	=	Integer(ls_Null)
	END IF
	
	IF iuo_buscadatosproceso.Packing > 0 THEN
		dw_1.Object.capr_cespak[1]  	=	iuo_buscadatosproceso.Packing
	ELSE
		dw_1.Object.capr_cespak[1]  	=	Integer(ls_Null)
	END IF	
	
	IF Not iuo_embalajesprod.Existe(iuo_buscadatosproceso.Cliente,dw_1.Object.emba_codigo[1],False,Sqlca) THEN
		dw_1.Object.emba_codigo[1]	=	ls_Null
	END IF
	
	IF Not iuo_productor.Existe(iuo_buscadatosproceso.Productor,False,Sqlca) THEN
		dw_1.Object.prod_codigo[1]	=	Long(ls_Null)
	ELSE
		dw_1.Object.prod_codigo[1]	=	iuo_buscadatosproceso.Productor
	END IF
	
	IF Not iuo_variedad.Existe(iuo_buscadatosproceso.Especie,iuo_buscadatosproceso.Variedad,False,Sqlca) THEN
		dw_1.Object.vari_codigo[1]		=	Integer(ls_Null)
		dw_1.Object.capr_varrot[1]		=	Integer(ls_Null)	
	ELSE
		dw_1.Object.vari_codigo[1]		=	iuo_buscadatosproceso.Variedad
		
		IF tab_1.tp_1.dw_3.Object.capr_varrot[ai_fila] <> 0 THEN ii_varrot =	tab_1.tp_1.dw_3.Object.capr_varrot[ai_fila]
		
		IF NOT IsNull(ii_varrot) THEN
			dw_1.Object.capr_varrot[1]	=	ii_varrot
		ELSE
			dw_1.Object.capr_varrot[1]	=	iuo_buscadatosproceso.Variedad
		END IF
			
	END IF
	
	dw_1.Object.espe_codigo[1]			=	iuo_buscadatosproceso.Especie
	dw_1.Object.capr_fecdig[1]			=	Date(Today())
	dw_1.Object.capr_hordig[1]			=	Time(Today())

	dw_1.Object.cate_codigo[1] 		=	ii_categoria//iuo_buscadatosproceso.Categoria
	dw_1.Object.etiq_codigo[1] 		=	ii_etiqueta	//iuo_buscadatosproceso.Etiqueta
	
	SELECT	zona_codigo
		INTO	:ii_zona
		FROM	dba.plantadesp
		WHERE plde_codigo = :li_planta;
		
		
END IF

RETURN TRUE
end function

public subroutine cargamaximo ();Integer	li_filas
Long		ll_maximo

FOR li_filas = 1 TO tab_1.tp_1.dw_3.RowCount()
	ll_maximo	=	ll_maximo + tab_1.tp_1.dw_3.Object.cajas[li_filas]
NEXT

hpb_impresion.SetRange(0, ll_maximo)
hpb_impresion.Position	=	0
hpb_impresion.Visible	=	True
end subroutine

on w_info_etiqetas_compactos.create
int iCurrent
call super::create
this.dw_7=create dw_7
this.dw_crea_caja=create dw_crea_caja
this.tab_1=create tab_1
this.hpb_impresion=create hpb_impresion
this.gb_3=create gb_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_7
this.Control[iCurrent+2]=this.dw_crea_caja
this.Control[iCurrent+3]=this.tab_1
this.Control[iCurrent+4]=this.hpb_impresion
this.Control[iCurrent+5]=this.gb_3
end on

on w_info_etiqetas_compactos.destroy
call super::destroy
destroy(this.dw_7)
destroy(this.dw_crea_caja)
destroy(this.tab_1)
destroy(this.hpb_impresion)
destroy(this.gb_3)
end on

event open;call super::open;iuo_especie	=	Create uo_especie
iuo_correl	=	Create uo_lotescorrelequipo_gr
iuo_etiq		=	Create uo_etiquetas
iuo_clie		=	Create uo_cliente

dw_1.SetTransObject(Sqlca)

tab_1.tp_1.dw_3.SetTransObject(Sqlca)
tab_1.tp_2.dw_4.SetTransObject(Sqlca)
tab_1.tp_3.dw_5.SetTransObject(Sqlca)
tab_1.tp_4.dw_6.SetTransObject(Sqlca)
dw_crea_caja.SetTransObject(Sqlca)

RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", &
				"ComputerName", RegString!,is_Computador)			

ParamTemporada(gstr_paramtempo)
idt_fechaIni 	= gstr_paramtempo.fechainicio
idt_fechater = gstr_paramtempo.fechatermino

dw_2.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)

iuo_variedades	=	Create uo_variedades
iuo_entidad		=	Create uo_entidades
end event

event ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta, ll_fila_1, li_proceso
Date		ldt_FechaProc
Long		ll_Numero
String	ls_Nombre
Integer	li_planta, li_especie, li_cliente, li_tipord

li_Planta		=	dw_2.Object.plde_codigo[1]
li_Especie	=	dw_2.Object.espe_codigo[1]
ll_Numero	=	dw_2.Object.ppre_numero[1]
li_Cliente		=	dw_2.Object.clie_codigo[1]

SetNull(ii_varrot)

ldt_FechaProc	=	Date(Mid(istr_Mant.Argumento[4], 1, 10))
istr_Mant.Argumento[13] = string(dw_2.object.clie_codigo[1])
idwc_variedad.Retrieve(Integer(istr_mant.argumento[2]))
DO
	ll_fila_e	=	dw_2.Retrieve(li_planta, li_especie, ll_numero, li_cliente)
										  
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
						Information!, RetryCancel!)		
	ELSEIF ll_fila_e < 1 THEN 
		Return
	ELSE
		DO
			
		IF tab_1.tp_1.dw_3.Retrieve(li_planta, li_cliente, li_especie, ldt_FechaProc, ll_numero, 0) = -1 OR &
			tab_1.tp_2.dw_4.Retrieve(li_planta, li_cliente, li_especie, ldt_FechaProc, ll_numero, 1) = -1 OR &
			tab_1.tp_3.dw_5.Retrieve(li_planta, li_cliente, li_especie, ldt_FechaProc, ll_numero, 2) = -1 OR &
			tab_1.tp_4.dw_6.Retrieve(li_planta, li_cliente, li_especie, ldt_FechaProc, ll_numero, 3) = -1 THEN

				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				dw_2.GetChild("orpr_numero", idwc_procprog)
				idwc_procprog.SetTransObject(Sqlca)
				IF idwc_procprog.Retrieve(li_planta, li_cliente, li_especie, ldt_FechaProc, ll_numero) > 0 THEN	li_proceso = idwc_procprog.GetItemNumber(1, "orpr_numero")
				
				dw_2.Object.orpr_numero[1] = li_proceso
				
				tab_1.tp_1.dw_3.SetFilter("orpr_numero = " + String(li_proceso) )
				tab_1.tp_2.dw_4.SetFilter("orden = " + String(li_proceso) )
				
				tab_1.tp_1.dw_3.Filter()
				tab_1.tp_2.dw_4.Filter()
				
				IF tab_1.tp_1.dw_3.RowCount() < 1 THEN RETURN
				
				li_tipord	=	tab_1.tp_1.dw_3.Object.orpr_tipord[1]
				
				tab_1.tp_1.dw_3.GetChild("capr_varrot", idwc_varrot)
				idwc_varrot.SetTransObject(Sqlca)
				
				iuo_buscadatosproceso	=	Create	uo_buscadatosproceso	
				
				iuo_buscadatosproceso.existe(li_planta, li_cliente, li_proceso,li_tipord,True,Sqlca)
				iuo_variedades.existe(Integer(istr_mant.argumento[2]), iuo_buscadatosproceso.Variedad,True,Sqlca) 											  
											  
				idwc_varrot.Retrieve(li_especie,  iuo_variedades.VariRelaci)
				dw_2.Object.vari_codigo[1]	=	iuo_buscadatosproceso.Variedad
				
				destroy iuo_buscadatosproceso;
				
				pb_imprimir.Enabled	=	True
				dw_2.Object.ppre_fecpro[1]	=	ldt_FechaProc
				il_Fila	=	1
				tab_1.tp_1.SetFocus()
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_seleccion;str_Busqueda	lstr_Busq

lstr_Busq.Argum[1]	=	String(dw_2.Object.plde_codigo[1])
lstr_Busq.Argum[2]	=	'0'
lstr_Busq.Argum[3]	=	'4'
lstr_Busq.Argum[4]	=	String(dw_2.Object.clie_codigo[1])

OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[3] <> "" THEN
	istr_mant.argumento[2] = lstr_busq.argum[4]
	istr_mant.argumento[3] = lstr_Busq.Argum[11]
	istr_mant.argumento[4] = lstr_busq.argum[5]

	dw_2.Object.plde_codigo[1]		=	Integer(lstr_Busq.Argum[3])
	dw_2.Object.espe_codigo[1]	=	Integer(lstr_Busq.Argum[4])
	dw_2.Object.ppre_numero[1]	=	Long(lstr_Busq.Argum[11])
	dw_2.Object.ppre_fecpro[1]	=	Date(Mid(lstr_busq.argum[5],1,10))
	
	IF ExistePrograma("ppre_numero",lstr_Busq.Argum[11]) THEN
	
		TriggerEvent("ue_recuperadatos")
	END IF
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_nuevo;Long		ll_modif1, ll_modif2

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

dw_1.Reset()
dw_7.Reset()

pb_eli_det.Enabled			=	False
pb_ins_det.Enabled			=	False
pb_grabar.Enabled				=	False
pb_eliminar.Enabled			=	False
pb_imprimir.Enabled			=	False
dw_2.Enabled					=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.SetFocus()

istr_Mant.Argumento[1]		=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]		=	String(gi_Codespecie)
istr_Mant.Argumento[3]		=  String(Today(),'dd/mm/yyyy')
dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta

SetNull(ii_varrot)
end event

event ue_imprimir;
CHOOSE CASE tab_1.SelectedTab
	CASE 1
		ciclo_compacto()
		
	CASE 2
		imprime_dw(tab_1.tp_2.dw_4)
		
	CASE 3
		imprime_dw(tab_1.tp_3.dw_5)		
		
	CASE 4
		imprime_dw(tab_1.tp_4.dw_6)
END CHOOSE
end event

event closequery;//
end event

event resize;call super::resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

IF dw_2.width > Tab_1.width THEN
	maximo		=	dw_2.width
ELSE
	Tab_1.width	=	This.WorkSpaceWidth() - 400
	maximo		=	Tab_1.width
END IF

dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					= 32
//dw_2.height			= This.WorkSpaceHeight() - dw_2.y - 41

Tab_1.x					= 37 + Round((maximo - Tab_1.width) / 2, 0)
Tab_1.y					= dw_2.height + 64 
Tab_1.height			= This.WorkSpaceHeight() - dw_2.height - 96 - gb_3.height

tab_1.tp_1.dw_3.Width	=	tab_1.Width - 30
tab_1.tp_1.dw_3.height	=	tab_1.height - 30

gb_3.x					= 37 + Round((maximo - gb_3.width) / 2, 0)
gb_3.y					= 20 + Tab_1.y + Tab_1.Height

hpb_impresion.x		= 37 + Round((maximo - hpb_impresion.width) / 2, 0)
hpb_impresion.y		= 400

li_posic_x				= This.WorkSpaceWidth() - 250
li_posic_y				= 300
end event

type dw_1 from w_mant_encab_deta`dw_1 within w_info_etiqetas_compactos
boolean visible = false
integer x = 3671
integer y = 1952
integer width = 1458
integer height = 1132
boolean enabled = false
string title = ""
string dataobject = "dw_mant_mues_spro_cajasprod_cajasgranel"
boolean hscrollbar = false
boolean resizable = true
boolean livescroll = false
end type

event dw_1::retrievestart;call super::retrievestart;RETURN 2
end event

type dw_2 from w_mant_encab_deta`dw_2 within w_info_etiqetas_compactos
integer x = 41
integer y = 52
integer width = 3365
integer height = 432
integer taborder = 90
string dataobject = "dw_sel_prograproceso_enca_procesos"
end type

event dw_2::itemchanged;call super::itemchanged;Integer	li_Nula,	li_planta, li_proceso, li_cliente, li_tipord
String	ls_Columna, ls_Nula
Date		ldt_FechaProc, ldt_FechaSistema, ldt_fecha

ls_Columna = dwo.Name

SetNull(li_Nula)
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(row,"clie_codigo",Integer(ls_Nula))
			RETURN 1
		ELSE
			IF Integer(data) <> 81 OR Integer(data) <> 15  THEN
//				 dw_2.Object.Camara.Visible	=	True
//				dw_2.Object.Camara.Visible	=	True
				dw_2.Object.Camara[1]			= ls_Nula
//				dw_2.Object.Camara.Enabled	=	True
//			ELSE
//				dw_2.Object.Camara.Visible	=	False
//				dw_2.Object.Camara.Visible	=	False
//				dw_2.Object.Camara.Enabled	=	False				
			END IF
			
			istr_mant.Argumento[13] = data
		END IF

	
	CASE "ppre_numero"
		IF NOT Existeprograma(ls_columna, data) THEN
			This.SetItem(row,ls_columna,li_Nula)
			RETURN 1
		ELSE
			istr_mant.argumento[3] = Data
		END IF	

	CASE "espe_codigo"	
		istr_mant.argumento[2] = ""
		IF NOT iuo_Especie.Existe(Integer(Data),True,SqlCa) THEN  
			istr_mant.argumento[2] = ""
			This.SetItem(row,"espe_codigo", li_Nula)
			This.SetFocus()
			RETURN 1
	   ELSE
			IF NOT Existeprograma(ls_columna, data) THEN
				This.SetItem(row,"espe_codigo", li_Nula)
				istr_mant.argumento[2] = ""
				This.SetFocus()
				RETURN 1
			ELSE
				istr_mant.argumento[2] = Data
			END IF	
		END IF	
	
	CASE "ppre_fecpro"
		ldt_FechaProc		=	Date(Mid(Data, 1, 10))
		SetNull(ldt_fecha)
		IF ldt_FechaProc > idt_fechater THEN
			MessageBox("Atención","Fecha de Movimiento no puede ser superior a Fecha de término de temporada.")
			This.SetItem(row,"ppre_fecpro", ldt_fecha)
			RETURN 1
		ELSE	
			IF ldt_FechaProc < idt_fechaini THEN
				MessageBox("Atención","Fecha de Movimiento no puede ser menor a Fecha de inicio de temporada.")
				This.SetItem(row,"ppre_fecpro", ldt_fecha)
				RETURN 1
			ELSE
				istr_Mant.Argumento[4]	=	String(Date(Mid(Data,1,10)))
				IF ExisteProcesos(ldt_FechaProc) THEN
					Parent.PostEvent("ue_recuperadatos")
				END IF	
			END IF	
		END IF
		
	CASE "orpr_numero"
		tab_1.tp_1.dw_3.SetFilter("orpr_numero = " + data )
		tab_1.tp_2.dw_4.SetFilter("orden = " + data )
		tab_1.tp_1.dw_3.Filter()
		tab_1.tp_2.dw_4.Filter()
		
		IF tab_1.tp_1.dw_3.RowCount() < 1 THEN Return 1
		
		li_tipord	=	tab_1.tp_1.dw_3.Object.orpr_tipord[1]
		
		tab_1.tp_1.dw_3.GetChild("capr_varrot", idwc_varrot)
		idwc_varrot.SetTransObject(Sqlca)
		
		iuo_buscadatosproceso	=	Create	uo_buscadatosproceso	
		
		iuo_buscadatosproceso.existe(This.Object.plde_codigo[row], &
											  Integer(istr_mant.Argumento[13]), &
											  Integer(data),li_tipord,True,Sqlca)
 		iuo_variedades.existe(Integer(istr_mant.argumento[2]), iuo_buscadatosproceso.Variedad,True,Sqlca) 											  
											  
		idwc_varrot.Retrieve(Integer(istr_mant.argumento[2]), iuo_variedades.VariRelaci)
		idwc_variedad.Retrieve(Integer(istr_mant.argumento[2]))
		dw_2.Object.vari_codigo[1] =  iuo_buscadatosproceso.Variedad	
		
		Destroy iuo_buscadatosproceso;
		
	CASE "emba_codigo"
		IF Data <> "" THEN
			tab_1.tp_1.dw_3.SetFilter("emba_codigo = '" + data + "'")
			tab_1.tp_3.dw_5.SetFilter("tipo = '" + data + "'")
		ELSE
			tab_1.tp_1.dw_3.SetFilter("")
			tab_1.tp_3.dw_5.SetFilter("")
		END IF
		
		tab_1.tp_1.dw_3.Filter()
		tab_1.tp_3.dw_5.Filter()
END CHOOSE
//
//HabilitaIngreso(ls_Columna)
end event

type pb_nuevo from w_mant_encab_deta`pb_nuevo within w_info_etiqetas_compactos
boolean visible = false
integer x = 3675
integer taborder = 30
boolean enabled = false
end type

type pb_eliminar from w_mant_encab_deta`pb_eliminar within w_info_etiqetas_compactos
boolean visible = false
integer x = 3675
integer taborder = 40
end type

type pb_grabar from w_mant_encab_deta`pb_grabar within w_info_etiqetas_compactos
boolean visible = false
integer x = 3675
integer taborder = 60
end type

type pb_imprimir from w_mant_encab_deta`pb_imprimir within w_info_etiqetas_compactos
integer x = 3675
integer taborder = 70
end type

event pb_imprimir::clicked;call super::clicked;ib_FlagRetrieve	=	False
end event

type pb_salir from w_mant_encab_deta`pb_salir within w_info_etiqetas_compactos
integer x = 3675
integer taborder = 80
end type

type pb_ins_det from w_mant_encab_deta`pb_ins_det within w_info_etiqetas_compactos
boolean visible = false
integer x = 3675
integer taborder = 100
end type

type pb_eli_det from w_mant_encab_deta`pb_eli_det within w_info_etiqetas_compactos
boolean visible = false
integer x = 3675
integer taborder = 110
end type

type pb_buscar from w_mant_encab_deta`pb_buscar within w_info_etiqetas_compactos
integer x = 3675
integer taborder = 20
end type

type dw_7 from datawindow within w_info_etiqetas_compactos
boolean visible = false
integer x = 3662
integer y = 1804
integer width = 1248
integer height = 1040
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_spro_cajasprod_gtin14_nvofto_wm"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_crea_caja from datawindow within w_info_etiqetas_compactos
boolean visible = false
integer x = 2030
integer y = 332
integer width = 1289
integer height = 108
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_creacion_cajas_granel"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type tab_1 from tab within w_info_etiqetas_compactos
integer x = 41
integer y = 548
integer width = 3392
integer height = 1260
integer taborder = 50
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
boolean powertips = true
integer selectedtab = 1
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
tp_4 tp_4
end type

on tab_1.create
this.tp_1=create tp_1
this.tp_2=create tp_2
this.tp_3=create tp_3
this.tp_4=create tp_4
this.Control[]={this.tp_1,&
this.tp_2,&
this.tp_3,&
this.tp_4}
end on

on tab_1.destroy
destroy(this.tp_1)
destroy(this.tp_2)
destroy(this.tp_3)
destroy(this.tp_4)
end on

type tp_1 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3355
integer height = 1132
long backcolor = 16711680
string text = "Compactos"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_3 dw_3
end type

on tp_1.create
this.dw_3=create dw_3
this.Control[]={this.dw_3}
end on

on tp_1.destroy
destroy(this.dw_3)
end on

type dw_3 from datawindow within tp_1
integer x = 5
integer y = 28
integer width = 3214
integer height = 1056
integer taborder = 20
string title = "none"
string dataobject = "dw_compactos"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_Nula
String	ls_Columna

ls_Columna = dwo.Name

SetNull(li_Nula)

CHOOSE CASE ls_Columna
	CASE "capr_varrot"
		iuo_buscadatosproceso	=	Create	uo_buscadatosproceso	
		
		iuo_buscadatosproceso.existe(dw_2.Object.plde_codigo[1], &
											  Integer(istr_mant.Argumento[13]), &
											  This.Object.orpr_numero[row],&
											  This.Object.orpr_tipord[row],True,Sqlca)
//		
//		IF NOT iuo_variedades.existevarrot(Integer(istr_mant.argumento[2]), iuo_buscadatosproceso.Variedad,&
//													  Integer(data), True,Sqlca) THEN
		
		IF NOT iuo_variedades.existe(Integer(istr_mant.argumento[2]),Integer(data), True,Sqlca) THEN
			This.Object.capr_varrot[row]	=	li_Nula
			SetNull(ii_varrot)
			
			Destroy iuo_buscadatosproceso;
			RETURN 1
		ELSEIF iuo_buscadatosproceso.Estado <> 5 Then
			ii_varrot							=	Integer(data)
		ELSE
			MessageBox("Atención","Proceso con Cierre Web, favor seleccione Otro N° de Proceso.", Exclamation!)
			This.Object.capr_varrot[row]	=	li_Nula
			SetNull(ii_varrot)
			Destroy iuo_buscadatosproceso;
			RETURN 1
		END IF
		
		Destroy iuo_buscadatosproceso;
		
	CASE "capr_embala"
		IF NOT iuo_entidad.existeembaladora(Long(data), True, sqlca) THEN
			This.Object.capr_embala[row]	=	li_nula
			RETURN 1
		END IF
		
	CASE "etiq_codigo"
		IF NOT iuo_etiq.Existe(Integer(data), True, sqlca) THEN
			This.Object.etiq_codigo[row]	=	li_nula
			RETURN 1			
		END IF
		
	CASE "cajas"
			iuo_buscadatosproceso	=	Create	uo_buscadatosproceso	
		
			iuo_buscadatosproceso.existe(dw_2.Object.plde_codigo[1], &
											  Integer(istr_mant.Argumento[13]), &
											  This.Object.orpr_numero[row],&
											  This.Object.orpr_tipord[row],True,Sqlca)
		
		IF iuo_buscadatosproceso.Estado = 5 Then
			MessageBox("Atención","Proceso con Cierre Web, favor seleccione Otro N° de Proceso.", Exclamation!)
			This.Object.cajas[row]	=	li_Nula
			Destroy iuo_buscadatosproceso;
			RETURN 1	
		END IF
END CHOOSE
end event

event itemerror;return 1
end event

type tp_2 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3355
integer height = 1132
long backcolor = 16711680
string text = "O.Proceso - Lote"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_4 dw_4
end type

on tp_2.create
this.dw_4=create dw_4
this.Control[]={this.dw_4}
end on

on tp_2.destroy
destroy(this.dw_4)
end on

type dw_4 from uo_dw within tp_2
integer x = 1115
integer y = 28
integer width = 1161
integer height = 1084
integer taborder = 11
string dataobject = "dw_mues_lotes"
end type

type tp_3 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3355
integer height = 1132
long backcolor = 16711680
string text = "Embalaje"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_5 dw_5
end type

on tp_3.create
this.dw_5=create dw_5
this.Control[]={this.dw_5}
end on

on tp_3.destroy
destroy(this.dw_5)
end on

type dw_5 from uo_dw within tp_3
integer x = 1221
integer y = 28
integer width = 951
integer height = 1080
integer taborder = 11
string dataobject = "dw_mues_embalajes_etiq"
end type

type tp_4 from userobject within tab_1
integer x = 18
integer y = 112
integer width = 3355
integer height = 1132
long backcolor = 16711680
string text = "Calibre"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_6 dw_6
end type

on tp_4.create
this.dw_6=create dw_6
this.Control[]={this.dw_6}
end on

on tp_4.destroy
destroy(this.dw_6)
end on

type dw_6 from uo_dw within tp_4
integer x = 1221
integer y = 28
integer width = 951
integer height = 1080
integer taborder = 21
string dataobject = "dw_mues_calibres_etiq"
end type

type hpb_impresion from hprogressbar within w_info_etiqetas_compactos
boolean visible = false
integer x = 73
integer y = 1900
integer width = 3241
integer height = 64
boolean bringtotop = true
unsignedinteger maxposition = 2
unsignedinteger position = 1
integer setstep = 1
boolean smoothscroll = true
end type

type gb_3 from groupbox within w_info_etiqetas_compactos
integer x = 41
integer y = 1828
integer width = 3301
integer height = 164
integer taborder = 120
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
string text = "Progreso Creación de Cajas"
end type

