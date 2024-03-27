$PBExportHeader$w_info_etiqetas_compactos_cajasprod.srw
$PBExportComments$Ventana de mantención de Orden de Proceso de Fruta Granel.
forward
global type w_info_etiqetas_compactos_cajasprod from w_mant_encab_deta
end type
type tab_1 from tab within w_info_etiqetas_compactos_cajasprod
end type
type tp_1 from userobject within tab_1
end type
type dw_camaraysdpsag from datawindow within tp_1
end type
type dw_3 from datawindow within tp_1
end type
type tp_1 from userobject within tab_1
dw_camaraysdpsag dw_camaraysdpsag
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
type tab_1 from tab within w_info_etiqetas_compactos_cajasprod
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
tp_4 tp_4
end type
type dw_7 from datawindow within w_info_etiqetas_compactos_cajasprod
end type
type dw_8 from datawindow within w_info_etiqetas_compactos_cajasprod
end type
type dw_crea_caja from datawindow within w_info_etiqetas_compactos_cajasprod
end type
type hpb_impresion from hprogressbar within w_info_etiqetas_compactos_cajasprod
end type
type gb_3 from groupbox within w_info_etiqetas_compactos_cajasprod
end type
end forward

global type w_info_etiqetas_compactos_cajasprod from w_mant_encab_deta
integer width = 4027
integer height = 2168
string title = "IMPRESIÓN DE COMPACTOS"
string menuname = ""
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
boolean center = true
tab_1 tab_1
dw_7 dw_7
dw_8 dw_8
dw_crea_caja dw_crea_caja
hpb_impresion hpb_impresion
gb_3 gb_3
end type
global w_info_etiqetas_compactos_cajasprod w_info_etiqetas_compactos_cajasprod

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
uo_cliente					iuo_clie
uo_voicecode				iuo_voicecode
uo_QR						iuo_QR

DataWindowChild  		idwc_pltadesp, idwc_tipdoc, idwc_numero, idwc_planta,&
                  				idwc_especie, idwc_variedad, idwc_serplanta, &
								idwc_linea, idwc_predio


Date 							idt_fechaIni, idt_fechater

Integer						ii_lote, ii_proceso, ii_contratista, ii_etiquetas, ii_totalcajas, ii_cajasactuales
String						is_embalaje, is_calibre, is_impresora
Integer						ii_zona, ii_Cliente, ii_Planta, ii_categoria, ii_etiqueta
Date							id_Fecha
Boolean						ib_impresora, ib_ImpresoraPrimerCambio
uo_manejoimpresora		iuo_impresora
end variables

forward prototypes
public function boolean noexistecliente (integer cliente)
public function boolean existeprograma (string as_columna, string as_valor)
public function boolean existeprocesos (date adt_fechaproc)
public subroutine imprime_dw (ref datawindow al_dwimprime)
public function boolean despieceregistro (string as_registro)
public function boolean buscadatosproceso (long al_proceso)
public function boolean buscanuevocorrelativo (integer ai_bodega, integer ai_cliente)
public subroutine imprime_compacto ()
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine ciclo_compacto ()
public function boolean validamaxcajas ()
public function string rescatadw ()
public function integer buscacontratistaembaladora (long al_embaladora)
end prototypes

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

public function boolean existeprograma (string as_columna, string as_valor);Long		ll_Numero
String	ls_Nombre
Boolean	lb_Retorno = True
Date		ldt_fecha
Integer	li_planta, li_especie, li_cliente

li_Planta		=	dw_2.Object.plde_codigo[1]
li_Especie	=	dw_2.Object.espe_codigo[1]
ll_Numero	=	dw_2.Object.ppre_numero[1]
li_Cliente		=	dw_2.Object.clie_codigo[1]

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
		FROM	dbo.spro_programaprocenca
		WHERE	plde_codigo	=	:li_Planta
		AND	espe_codigo	=	:li_Especie
		AND	ppre_numero	=	:ll_Numero
		AND   clie_codigo =  :li_Cliente;
		
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

li_Planta	=	dw_2.Object.plde_codigo[1]
li_Especie	=	dw_2.Object.espe_codigo[1]
ll_Numero	=	dw_2.Object.ppre_numero[1]
li_Cliente	=	dw_2.Object.clie_codigo[1]

SELECT	Count(sp.pprd_secuen)
	INTO	:li_Cantidad
	FROM	dbo.spro_progordenproceso as sp, 
			dbo.spro_programaprocenca as se
	WHERE	sp.plde_codigo	=	:li_Planta
	AND	sp.espe_codigo	=	:li_Especie
	AND	sp.ppre_numero	=	:ll_Numero
	AND   se.clie_codigo =  :li_Cliente	
	AND	sp.popr_fecpro	=	:adt_FechaProc
	AND   sp.plde_codigo	=	se.plde_codigo
	AND	sp.espe_codigo	=	se.espe_codigo
	AND	sp.ppre_numero	=  se.ppre_numero
	AND	se.clie_codigo	=	:li_Cliente;
	
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



FOR ll_filas = 1 to al_dwimprime.RowCount()
	ll_proceso	=	al_dwimprime.Object.orden[ll_filas]
	ls_texto		=	al_dwimprime.Object.tipo[ll_filas]
	ls_texto		=	String( ( tab_1.SelectedTab ) , "00" ) + ls_texto
	ll_cajas		=	al_dwimprime.Object.cajas[ll_filas]
	FOR ll_etiquetas = 1 to ll_cajas
		ll_Trabajo	=	PrintOpen()
		Print(ll_Trabajo, "qC")
		Print(ll_Trabajo, "n")
		Print(ll_Trabajo, "e")
		Print(ll_Trabajo, "c0000")
		Print(ll_Trabajo, "RN")
		Print(ll_Trabajo, "Kf0000")
		Print(ll_Trabajo, "V0")
		Print(ll_Trabajo, "M0635")
		Print(ll_Trabajo, "L")
		Print(ll_Trabajo, "A2")
		Print(ll_Trabajo, "D11")
		Print(ll_Trabajo, "z")
		Print(ll_Trabajo, "PG")
		Print(ll_Trabajo, "SG")
		Print(ll_Trabajo, "H14")
		Print(ll_Trabajo, "4A6305000750085" + ls_texto)
		//4A landscape
		// 630
		// 920-500 altura codigo
		// 075-113 ubicacion y
		// 115-225 ubicacion X
		//Print(ll_Trabajo, "4A 630 920 075 0115" + ls_texto)
		//Print(ll_Trabajo, "4A 630 500 075 0115" + ls_texto)
		//Print(ll_Trabajo, "4A 630 500 113 0225" + String(ll_proceso))
		IF ll_proceso > 0 THEN
							   // "4A 950 500 075 0085"
								//"4A 950 500 115 0160"
		 	Print(ll_Trabajo, "4A6305000750160"+"01" + String(ll_proceso))
		END IF
		Print(ll_Trabajo, "^01")
		Print(ll_Trabajo, "Q0001")
		Print(ll_Trabajo, "E")
		PrintClose(ll_Trabajo)
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

public function boolean buscadatosproceso (long al_proceso);Integer	li_Planta
String	ls_Null

SetNull(ls_Null)

iuo_buscadatosproceso	=	Create uo_buscadatosproceso	
iuo_embalajesprod			=	Create uo_embalajesprod	
iuo_productor				=	Create uo_productores
iuo_variedad				=	Create uo_variedades

IF iuo_buscadatosproceso.existe(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[4]), Long(istr_mant.argumento[3]),Integer(istr_mant.argumento[8]),True,Sqlca)	THEN

	iuo_buscadatosproceso.buscapredio(ii_lote,False,Sqlca)

	iuo_buscadatosproceso.buscaplanta(iuo_buscadatosproceso.Planta,False,Sqlca)

	iuo_buscadatosproceso.buscacatetiq(al_proceso,False,Sqlca)

	dw_1.Object.clie_codigo[1]			=	iuo_buscadatosproceso.Cliente
	dw_1.Object.plde_codigo[1]			=	iuo_buscadatosproceso.Planta
	dw_1.Object.capr_fecemb[1] 		=	iuo_buscadatosproceso.Fecha

	li_Planta								=	iuo_buscadatosproceso.Planta

	iuo_ParamPrecos						=	Create uo_Paramprecos       

	IF	iuo_buscadatosproceso.Predio > 0 THEN
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
		dw_1.Object.vari_codigo[1]	=	Integer(ls_Null)
		dw_1.Object.capr_varrot[1]	=	Integer(ls_Null)	
	ELSE
		dw_1.Object.vari_codigo[1]	=	iuo_buscadatosproceso.Variedad
		dw_1.Object.capr_varrot[1]	=	iuo_variedad.varirelaci 		
	END IF

	dw_1.Object.espe_codigo[1]		=	iuo_buscadatosproceso.Especie
	dw_1.Object.capr_fecdig[1]		=	Date(Today())
	dw_1.Object.capr_hordig[1]		=	Time(Today())

	dw_1.Object.cate_codigo[1]		=	ii_categoria	//iuo_buscadatosproceso.Categoria
	dw_1.Object.etiq_codigo[1]		=	ii_etiqueta		//iuo_buscadatosproceso.Etiqueta

	SELECT	zona_codigo
		INTO	:ii_zona
		FROM	dbo.plantadesp
		WHERE plde_codigo = :li_planta;

	dw_1.Object.capr_numpal[1]		=	Long(istr_mant.argumento[9])		
	dw_1.Object.capr_embala[1]		=	Integer(istr_mant.argumento[11]) * -1
	dw_1.Object.capr_pesado[1]		=	Integer(istr_mant.argumento[12])
	dw_1.Object.capr_tipdoc[1]		=	Integer(istr_mant.argumento[8])
	dw_1.Object.capr_pcline[1]		=	gstr_us.computador
	dw_1.Object.capr_lineas[1]		=	iuo_correl.loco_comlin
	dw_1.Object.cont_codigo[1]		=	ii_contratista
END IF

RETURN TRUE
end function

public function boolean buscanuevocorrelativo (integer ai_bodega, integer ai_cliente);Long	ll_Numero
SetPointer(HourGlass!)

IF NOT iuo_correl.ExisteCorrel(Integer(istr_mant.argumento[1]),99, &
								gstr_us.computador, TRUE, sqlca) THEN						
	RETURN FALSE
ELSE
	dw_1.Object.capr_numero[1]			=	iuo_correl.il_correcompa
	dw_7.DataObject						=	iuo_correl.loco_dwcomp
	dw_7.SetTransObject(SQLCA)
	
	IF istr_mant.argumento[2] = '11' THEN 
		IF istr_mant.argumento[10] = '3' THEN//England
			dw_7.Modify("l_7.Visible = 1")
			dw_7.Modify("l_8.Visible = 1")
			dw_7.Modify("t_3.Visible = 0")
			dw_7.Modify("t_4.Visible = 0")
			dw_7.Modify("l_9.Visible = 0")
			dw_7.Modify("l_17.Visible = 0")
		ELSEIF istr_mant.argumento[10] = '1' THEN//USA
			dw_7.Modify("capr_calibr_1.Visible = 0")
			dw_7.Modify("capr_calibr_2.Visible = 0")
		END IF
	END IF
	
	RETURN True
END IF
end function

public subroutine imprime_compacto ();Long		ll_Fila, ll_recorre, ll_inicial, ll_final, ll_numero, ll_Productor
String		ls_codigo, ls_fecha, ls_CSG, ls_QR, ls_Calibre, ls_Embalaje, ls_Ruta
uo_Destinos	luo_Destinos 

luo_Destinos 	=	Create uo_Destinos

SetPointer(HourGlass!)

dw_8.SetSort("pafr_cajori desc, pafr_secuen")
dw_8.Sort()

ll_numero	=	dw_8.Find("pafr_cajori = 0", 1, dw_8.RowCount())

If dw_8.RowCount() 	< 	1 Then RETURN
If ll_numero 			= 	0 Then RETURN

SetPointer(HourGlass!)

FOR ll_recorre = ll_numero TO dw_8.RowCount() 
	If dw_8.Object.pafr_cajori[ll_recorre] = 0 Then
		
		ll_inicial	=	ll_recorre
		ll_final		=	iuo_correl.foad_canocx + (ll_recorre - 1)
	
		If ll_final > dw_8.RowCount() Then
			ll_final = dw_8.RowCount()
		End If

		dw_8.Object.pafr_cajori[ll_inicial] =	1
		dw_8.Object.pafr_cajori[ll_final] 	= 	1
		
		iuo_clie.Existe(Integer(istr_mant.argumento[4]), False, sqlca)
		luo_Destinos.Existe(Integer(istr_mant.argumento[10]), False, sqlca)

		If istr_mant.argumento[4] <> '81' Then
			ll_Fila = dw_7.Retrieve(dw_1.Object.clie_codigo[1], dw_1.Object.plde_codigo[1],&
									 Long(dw_8.Object.pafr_secuen[ll_inicial]), Long(dw_8.Object.pafr_secuen[ll_final]),1,&
											tab_1.tp_1.dw_camaraysdpsag.Object.camara[1], tab_1.tp_1.dw_camaraysdpsag.Object.sdpsag[1])

			If iuo_correl.foad_canocx = 1 Then

			End If

		Else
			ll_Fila = dw_7.Retrieve(dw_1.Object.clie_codigo[1], dw_1.Object.plde_codigo[1], &
									 Long(dw_8.Object.pafr_secuen[ll_inicial]), Long(dw_8.Object.pafr_secuen[ll_final]),	 luo_Destinos.Mercado)
		End If

		If ll_Fila > 0 Then
			If  ii_etiquetas > 0 Then
				ls_fecha			=	String(dw_7.Object.capr_fecemb[1])
				ls_fecha			=	Right(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Left(ls_fecha, 2)
				
				If IsNull(dw_7.Object.prpr_prepro[1]) Or dw_7.Object.prpr_prepro[1] = "" Then 
					ls_CSG = "0000000"
				Else
					ls_CSG	= Fill('0', 7 - Len(String(dw_7.Object.prpr_prepro[1]))) + dw_7.Object.prpr_prepro[1]
				End If

				ls_codigo			=	"01" + dw_7.Object.emba_nroint[1] + "13" + ls_fecha + "\F"
				ls_codigo			=	ls_codigo	 +	"10" + ls_CSG  + "\F"
				ls_codigo			=	ls_codigo	+	"21" + String(dw_7.Object.plde_codigo[1], "0000") 
				ls_codigo			=	ls_codigo	+	String(dw_7.Object.capr_numero[1], '00000000') + "\F"
	
				dw_7.Object.Ole_1.Object.BarCode	=	iuo_clie.Barras
				dw_7.Object.Ole_1.Object.Text 		= 	ls_codigo
				/*
				Codigo QR
				*/		
				ls_Embalaje = dw_7.Object.emba_codigo[1]
				ls_Calibre	= dw_7.Object.capr_calibr[1]
				
				ls_QR	= 'Productor:	' + String(ll_Productor, '00000') + Char(10) 
				ls_QR	+= 'Especie: 	' + String(dw_7.Object.espe_codigo[1], '00') + ' / ' + String(dw_7.Object.vari_codigo[1], '0000')  + Char(10) 
				ls_QR	+= 'Predio: ' + ls_CSG + ' / ' + String(dw_7.Object.prod_cuarte[1],'000' )  + Char(10)
				ls_QR	+= 'Emb.: 	' + ls_Embalaje + ' / ' + ls_Calibre + Char(10)
				ls_QR	+= 'Fecha Emb.: ' + String(dw_7.Object.capr_fecemb[1], 'dd/mm/yyyy')
				
				ls_Ruta = iuo_QR.of_genera_qr(ls_QR)
				dw_7.Object.p_qrcode.FileName = ls_Ruta
				
				//Codigo UPC
				dw_7.Object.Ole_UPC.Object.Text 	= 	dw_7.Object.gtin_codupc[1]
					
				/*
				Code Pick Voice
				 */
				iuo_voicecode	=	Create uo_voicecode
				iuo_voicecode.voicecode(dw_7	, dw_7.Object.emba_nroint[1],  ls_fecha, dw_7.Object.capr_fecemb[1], iuo_correl.foad_vopico)
				ii_etiquetas								=	ii_etiquetas - 1
			Else
				dw_7.Object.Ole_1.Object.BarCode		=	iuo_clie.Barras
				dw_7.Object.Ole_1.Object.Text 		= 	"0"				
			End If

			If iuo_correl.foad_canocx = 2 AND dw_7.RowCount() > 1 Then
				If  ii_etiquetas > 0 Then
					ls_fecha			=	String(dw_7.Object.capr_fecemb[2])
					ls_fecha			=	Right(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Left(ls_fecha, 2)
					
					If IsNull(dw_7.Object.prpr_prepro[1]) Or dw_7.Object.prpr_prepro[1] = "" Then 
						ls_CSG = "0000000"
					Else
						ls_CSG	= Fill('0', 7 - Len(String(dw_7.Object.prpr_prepro[1]))) + dw_7.Object.prpr_prepro[1]
					End If

					ls_codigo			=	"01" + dw_7.Object.emba_nroint[1] + "13" + ls_fecha + "\F"
					ls_codigo			=	ls_codigo	 +	"10" + ls_CSG  + "\F"
					ls_codigo			=	ls_codigo	+	"21" + String(dw_7.Object.plde_codigo[1], "0000") 
					ls_codigo			=	ls_codigo	+	String(dw_7.Object.capr_numero[1], '00000000') + "\F"
	
					dw_7.Object.Ole_2.Object.BarCode		=	iuo_clie.Barras
					dw_7.Object.Ole_2.Object.Text 			= 	ls_codigo
					
					/*
					Codigo QR
					*/		
					ls_Embalaje = dw_7.Object.emba_codigo[1]
					ls_Calibre	= dw_7.Object.capr_calibr[1]
					
					ls_QR	= 'Productor:	' + String(ll_Productor, '00000') + Char(10) 
					ls_QR	+= 'Especie: 	' + String(dw_7.Object.espe_codigo[1], '00') + ' / ' + String(dw_7.Object.vari_codigo[1], '0000')  + Char(10) 
					ls_QR	+= 'Predio: ' + ls_CSG + ' / ' + String(dw_7.Object.prod_cuarte[1],'000' )  + Char(10)
					ls_QR	+= 'Emb.: 	' + ls_Embalaje + ' / ' + ls_Calibre + Char(10)
					ls_QR	+= 'Fecha Emb.: ' + String(dw_7.Object.capr_fecemb[1], 'dd/mm/yyyy')
					
					ls_Ruta = iuo_QR.of_genera_qr(ls_QR)
					dw_7.Object.p_qrcode.FileName = ls_Ruta
					
					//Codigo UPC
					dw_7.Object.Ole_UPC.Object.Text 	= 	dw_7.Object.gtin_codupc[1]
					
					/* Code Pick Voice */
					iuo_voicecode	=	Create uo_voicecode
					iuo_voicecode.voicecode(dw_7	, dw_7.Object.emba_nroint[1],  ls_fecha, dw_7.Object.capr_fecemb[1], iuo_correl.foad_vopico)
					
					ii_etiquetas								=	ii_etiquetas - 1
				Else
					dw_7.Object.Ole_2.Object.BarCode	=	iuo_clie.Barras
					dw_7.Object.Ole_2.Object.Text 		= 	""
				End If
			End If

			If iuo_correl.foad_canocx = 3  AND dw_7.RowCount() > 2 Then
				If  ii_etiquetas > 0 Then
					ls_fecha			=	String(dw_7.Object.capr_fecemb[3])
					ls_fecha			=	Right(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Left(ls_fecha, 2)
					
					If IsNull(dw_7.Object.prpr_prepro[1]) Or dw_7.Object.prpr_prepro[1] = "" Then 
						ls_CSG = "0000000"
					Else
						ls_CSG	= Fill('0', 7 - Len(String(dw_7.Object.prpr_prepro[1]))) + dw_7.Object.prpr_prepro[1]
					End If

					ls_codigo			=	"01" + dw_7.Object.emba_nroint[1] + "13" + ls_fecha + "\F"
					ls_codigo			=	ls_codigo	 +	"10" + ls_CSG  + "\F"
					ls_codigo			=	ls_codigo	+	"21" + String(dw_7.Object.plde_codigo[1], "0000") 
					ls_codigo			=	ls_codigo	+	String(dw_7.Object.capr_numero[1], '00000000') + "\F"

	
					dw_7.Object.Ole_3.Object.BarCode	=	iuo_clie.Barras
					dw_7.Object.Ole_3.Object.Text 		= 	ls_codigo
					/*
					Codigo QR
					*/		
					ls_Embalaje = dw_7.Object.emba_codigo[1]
					ls_Calibre	= dw_7.Object.capr_calibr[1]
					
					ls_QR	= 'Productor:	' + String(ll_Productor, '00000') + Char(10) 
					ls_QR	+= 'Especie: 	' + String(dw_7.Object.espe_codigo[1], '00') + ' / ' + String(dw_7.Object.vari_codigo[1], '0000')  + Char(10) 
					ls_QR	+= 'Predio: ' + ls_CSG + ' / ' + String(dw_7.Object.prod_cuarte[1],'000' )  + Char(10)
					ls_QR	+= 'Emb.: 	' + ls_Embalaje + ' / ' + ls_Calibre + Char(10)
					ls_QR	+= 'Fecha Emb.: ' + String(dw_7.Object.capr_fecemb[1], 'dd/mm/yyyy')
					
					ls_Ruta = iuo_QR.of_genera_qr(ls_QR)
					dw_7.Object.p_qrcode.FileName = ls_Ruta
					
					//Codigo UPC
					dw_7.Object.Ole_UPC.Object.Text 	= 	dw_7.Object.gtin_codupc[1]
					
					/* Code Pick Voice */
					iuo_voicecode	=	Create uo_voicecode
					iuo_voicecode.voicecode(dw_7	, dw_7.Object.emba_nroint[1],  ls_fecha, dw_7.Object.capr_fecemb[1], iuo_correl.foad_vopico)
					ii_etiquetas								=	ii_etiquetas - 1
				Else
					dw_7.Object.Ole_3.Object.BarCode		=	iuo_clie.Barras
					dw_7.Object.Ole_3.Object.Text 		= 	""
				End If
			End If
		Else
			MessageBox("Error Compactos", "No es posible recuperar el compacto para realizar su impresion", StopSign!)
			dw_8.SetSort("pafr_secuen asc")
			dw_8.Sort()
			Return
		End If
		
		If IsNull(is_impresora) OR Len(String(is_impresora)) < 1 Then
			ib_impresora	=	False
		Else
			iuo_impresora.asignaimpresora_comp(is_impresora)
			ib_impresora	=	True
		End If
	
		If iuo_impresora.is_impresoracomp <> '' AND ib_impresora Then
			iuo_impresora.setimprcomp()
			
			dw_7.AcceptText()
			dw_7.Print(False, False)
			
			iuo_impresora.setimprdef()
		Else
			dw_7.AcceptText()
			dw_7.Print(False, False)
		End If
		ll_recorre	=	iuo_correl.foad_canocx + (ll_recorre - 1)
	End If
NEXT

dw_8.SetSort("pafr_secuen asc")
dw_8.Sort()

Destroy luo_Destinos

SetPointer(Arrow!)
pb_Salir.PostEvent(Clicked!)
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);long ll_filasdetalle

If dw_1.UpDate() = 1 Then 
	Commit;
	
	dw_1.ResetUpdate()
	If sqlca.sqlcode <> 0 Then
		F_ErrorBaseDatos(sqlca,This.title)
		Return False
	Else
		//
		If dw_1.RowCount() > 0 Then
			ll_FilasDetalle										=	dw_8.InsertRow(0)
			dw_8.Object.pafr_secuen[ll_filasdetalle]		=	dw_1.Object.capr_numero[1]
			dw_8.Object.emba_codigo[ll_filasdetalle]	=	dw_1.Object.emba_codigo[1]
			dw_8.Object.pafr_calibr[ll_filasdetalle]		=	dw_1.Object.capr_calibr[1]
			dw_8.Object.pafr_calrot[ll_filasdetalle]		=	dw_1.Object.capr_calrot[1]
			dw_8.Object.lote_codigo[ll_filasdetalle]		=	dw_1.Object.capr_nrlote[1]
			dw_8.Object.pafr_ccajas[ll_filasdetalle]		=	1
			dw_8.Object.pafr_cajori[ll_filasdetalle]		=	1
			dw_8.Object.pafr_copack[ll_filasdetalle]		=	dw_8.Object.plde_codigo[ll_filasdetalle]
			dw_8.Object.pafr_tipdoc[ll_filasdetalle]		=	Integer(istr_mant.argumento[8])
			dw_8.Object.pafr_docrel[ll_filasdetalle]		=	Long(istr_mant.argumento[3])
			dw_8.Object.plde_origen[ll_filasdetalle]		=	dw_8.Object.plde_codigo[ll_filasdetalle]
			dw_8.Object.espe_codigo[ll_filasdetalle]		=	Integer(istr_mant.argumento[2])
			dw_8.Object.pafr_cajori[ll_filasdetalle]		=	0
			
			dw_8.Object.pafr_tipdoc[ll_filasdetalle]		=	dw_1.Object.capr_tipdoc[1]
			dw_8.Object.pafr_varrot[ll_filasdetalle]		=	dw_1.Object.capr_varrot[1]
			dw_8.Object.vari_codrot[ll_filasdetalle]		=	dw_1.Object.capr_varrot[1]
	
			dw_8.Object.prbr_codpre[ll_filasdetalle]		=	dw_1.Object.prod_huerto[1]
			dw_8.Object.prcc_codigo[ll_filasdetalle]		=	dw_1.Object.prod_cuarte[1]
			dw_8.Object.pafr_huert1[ll_filasdetalle]		=	dw_1.Object.prod_huerto[1]
			dw_8.Object.pafr_cuart1[ll_filasdetalle]		=	dw_1.Object.prod_cuarte[1]
			dw_8.Object.cont_codigo[ll_filasdetalle]		=	dw_1.Object.cont_codigo[1]
			dw_8.Object.pafr_ggncod[ll_filasdetalle]		=	f_AsignaGGN(dw_1.Object.prod_codigo[1], dw_1.Object.prod_huerto[1], Integer(istr_mant.argumento[2]), Date(istr_mant.argumento[5]))
			ii_cajasactuales									=	ii_cajasactuales + 1
			//
			
			If IsValid(w_maed_movtofrutaemba_proceso_cajasprod) Then
				If ii_totalcajas = ii_cajasactuales Then
					w_maed_movtofrutaemba_proceso_cajasprod.TriggerEvent("ue_guardar")
				End If
			End If
		End If
		Return True
	End If
Else
	Rollback;
	If sqlca.sqlcode <> 0 Then F_ErrorBaseDatos(sqlca, This.Title)
	Return false
End If

Return True
end function

public subroutine ciclo_compacto ();Integer 	li_filas, li_etiquetas, li_compactos, li_contratista, li_tipo, li_varrot, li_fila
Long		ll_proceso, ll_lote, ll_embala
String		ls_embalaje, ls_calibre, ls_Registro, ls_calibrerot

If Not validamaxcajas() Then Return

iuo_correl.ExisteCorrel(Integer(istr_mant.argumento[1]), 99, gstr_us.computador, FALSE, sqlca)						

Tab_1.Tp_1.dw_3.AcceptText()

For li_filas = 1 To Tab_1.Tp_1.dw_3.RowCount()
	If Tab_1.Tp_1.dw_3.Object.cajas[li_filas] > 0 Then
		ll_proceso		=	Tab_1.Tp_1.dw_3.Object.orpr_numero[li_filas]
		li_tipo				=	Tab_1.Tp_1.dw_3.Object.orpr_tipord[li_filas]
		ll_lote				=	Long(Tab_1.Tp_1.dw_3.Object.lote_codigo[li_filas])
		ls_embalaje		=	Tab_1.Tp_1.dw_3.Object.emba_codigo[li_filas]
		ls_calibre		=	Tab_1.Tp_1.dw_3.Object.caen_calibr[li_filas]
		ls_calibrerot		=	Tab_1.Tp_1.dw_3.Object.caen_calrot[li_filas]
		ii_categoria		=	Tab_1.Tp_1.dw_3.Object.cate_codigo[li_filas]
		ii_etiqueta		=	Tab_1.Tp_1.dw_3.Object.etiq_codigo[li_filas]	
		li_etiquetas		=	Tab_1.Tp_1.dw_3.Object.cajas[li_filas]
		li_varrot			=	Tab_1.Tp_1.dw_3.Object.capr_varrot[li_filas]
		ll_embala		=	Tab_1.Tp_1.dw_3.Object.capr_embala[li_filas]
		ii_etiquetas		=	li_etiquetas
		
		dw_crea_caja.Reset()
		dw_1.Reset()
		dw_7.Reset()
		For li_compactos = 1 To li_etiquetas
			li_fila	=	dw_crea_caja.Retrieve(Integer(istr_mant.argumento[4]), Long(istr_mant.argumento[1]), &
														 	 li_tipo, ll_proceso, li_varrot,ls_embalaje,ls_calibre,ii_etiqueta,&
															 ll_embala,gstr_us.computador, ii_categoria,ls_calibrerot)
			If li_fila = 1 Then
				If dw_crea_caja.Object.clie_codigo[1] <> -1 Then
					is_impresora					=	dw_crea_caja.Object.impresora[1]
					dw_7.DataObject				=	dw_crea_caja.Object.formato[1]
					dw_7.SetTransObject(SQLCA)
					li_fila								=	dw_1.Retrieve(Integer(istr_mant.argumento[4]), &
									     					Long(istr_mant.argumento[1]), dw_crea_caja.Object.nrocaja[1])
					If li_fila > 0 Then
						This.TriggerEvent("ue_guardar")
						hpb_impresion.StepIt()
					Else
						MessageBox("Error", "Imposible Recuperar compacto. Favor dar aviso a Administrador del Sistema.")
						This.TriggerEvent("ue_imprimirerror")
					End If
				Else
					MessageBox("Error", "No se puede Generar Compacto.~r~n" + String(dw_crea_caja.Object.formato[1]))
					This.TriggerEvent("ue_imprimirerror")
				End If
			Else
				MessageBox("Error", "No se puede Generar Compacto.")
				This.TriggerEvent("ue_imprimirerror")
			End If
			commit;
		Next
		Tab_1.Tp_1.dw_3.Object.cajas[li_filas] = 	0
		hpb_impresion.Visible						=	False
		Imprime_Compacto()
	End If
Next
end subroutine

public function boolean validamaxcajas ();Integer li_Cajas, li_CajasMax, li_fila, li_cajas_a_emitir

li_CajasMax 		= 	Integer(istr_mant.argumento[6])
ii_totalcajas		=	dw_8.RowCount()
ii_cajasactuales	=	0

FOR li_fila = 1 TO tab_1.tp_1.dw_3.RowCount()
	
	ii_totalcajas		=	ii_totalcajas 		+ 	tab_1.tp_1.dw_3.Object.cajas[li_fila]
	li_cajas_a_emitir	=	li_cajas_a_emitir	+ 	tab_1.tp_1.dw_3.Object.cajas[li_fila]
NEXT

IF ii_totalcajas <= li_CajasMax THEN
	hpb_impresion.SetRange(0, li_cajas_a_emitir)
	hpb_impresion.Position	=	0
	hpb_impresion.Visible	=	True
	
	RETURN TRUE
ELSE
	MessageBox("Error", "La cantidad de cajas ya ingresadas mas las que desea generar~n~res mayor a la cantidad determinada por el tipo de pallet", Exclamation!)
	RETURN FALSE
END IF


end function

public function string rescatadw ();return " "
end function

public function integer buscacontratistaembaladora (long al_embaladora);Integer	li_contratista
  
  SELECT cont_codigo
    INTO :li_contratista  
    FROM dbo.spro_entidadespacking
   WHERE enpa_codper = :al_embaladora
	USING sqlca;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla spro_entidadespacking")
	Return -1
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Embaladora No ha sido Generado. Ingrese Otro.")
	Return -1
END IF

Return li_contratista
end function

on w_info_etiqetas_compactos_cajasprod.create
int iCurrent
call super::create
this.tab_1=create tab_1
this.dw_7=create dw_7
this.dw_8=create dw_8
this.dw_crea_caja=create dw_crea_caja
this.hpb_impresion=create hpb_impresion
this.gb_3=create gb_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
this.Control[iCurrent+2]=this.dw_7
this.Control[iCurrent+3]=this.dw_8
this.Control[iCurrent+4]=this.dw_crea_caja
this.Control[iCurrent+5]=this.hpb_impresion
this.Control[iCurrent+6]=this.gb_3
end on

on w_info_etiqetas_compactos_cajasprod.destroy
call super::destroy
destroy(this.tab_1)
destroy(this.dw_7)
destroy(this.dw_8)
destroy(this.dw_crea_caja)
destroy(this.hpb_impresion)
destroy(this.gb_3)
end on

event open;/*
	istr_mant.argumento[1]	=	Planta
	istr_mant.argumento[2]	=	Especie
	istr_mant.argumento[3]	=	Proceso
	istr_mant.argumento[4]	=	Cliente
	istr_mant.argumento[5]	=	Fecha
	istr_mant.argumento[6]	=	Cajas
	istr_mant.argumento[7]	=	Embalaje
	istr_mant.argumento[8] 	= 	tipo documento
	istr_mant.argumento[9] 	= 	pallet padre
	lstr_mant.argumento[10]=	Destino
*/
iuo_especie		=	Create uo_especie
iuo_correl		=	Create uo_lotescorrelequipo_gr
iuo_clie			=	Create uo_cliente
iuo_impresora	=	Create uo_manejoimpresora
iuo_QR			=	Create uo_QR

istr_mant		=	Message.PowerObjectParm	
PostEvent("ue_recuperadatos")

istr_mant.dw.ShareData(dw_8)

dw_1.SetTransObject(Sqlca)
dw_8.SetTransObject(Sqlca)

tab_1.tp_1.dw_3.SetTransObject(Sqlca)
tab_1.tp_2.dw_4.SetTransObject(Sqlca)
tab_1.tp_3.dw_5.SetTransObject(Sqlca)
tab_1.tp_4.dw_6.SetTransObject(Sqlca)
dw_crea_caja.SetTransObject(Sqlca)

ib_ImpresoraPrimerCambio		=	False

tab_1.tp_1.dw_camaraysdpsag.InsertRow(0)

tab_1.tp_1.dw_3.getChild('capr_varrot', idwc_variedad)
idwc_variedad.SetTransObject(Sqlca)
If idwc_variedad.Retrieve(Integer(istr_mant.Argumento[2]), Integer(istr_mant.Argumento[13])) = -1 Then idwc_variedad.InsertRow(0)

IF istr_mant.argumento[2] = '11' THEN 
//	dw_7.DataObject = "dw_info_spro_cajasprod_uvas"
	IF istr_mant.argumento[10] = '3' THEN//England
		//dw_7.DataObject = "dw_info_spro_cajasprod_uvas_eng"
		dw_7.Modify("l_7.Visible = 1")
		dw_7.Modify("l_8.Visible = 1")
		dw_7.Modify("t_3.Visible = 0")
		dw_7.Modify("t_4.Visible = 0")
		dw_7.Modify("l_9.Visible = 0")
		dw_7.Modify("l_17.Visible = 0")
	END IF
	IF istr_mant.argumento[10] = '1' THEN//USA
		//dw_7.DataObject = "dw_info_spro_cajasprod_uvas_usa"
		dw_7.Modify("capr_calibr_1.Visible = 0")
		dw_7.Modify("capr_calibr_2.Visible = 0")
	END IF
ELSE
	//IF istr_mant.argumento[2] = '21' THEN 	dw_7.DataObject = "dw_info_spro_cajasprod_emer"
	IF istr_mant.argumento[2] = '21' THEN 	dw_7.DataObject = "dw_info_spro_cajasprod_carozos"
END IF

dw_7.SetTransObject(Sqlca)

Paramtemporada(gstr_paramtempo)
idt_fechaIni = gstr_paramtempo.fechainicio
idt_fechater = gstr_paramtempo.fechatermino
tab_1.tp_1.dw_camaraysdpsag.Object.camara[1] = ' '


end event

event ue_recuperadatos;Long		ll_fila_d, ll_fila_e, respuesta, ll_fila_1
Date		ldt_FechaProc
Long		ll_Numero
String	ls_Nombre
Integer	li_planta, li_especie, li_cliente, li_fila

li_Planta		=		Integer(istr_mant.argumento[1])
li_Especie	=		Integer(istr_mant.argumento[2])
ll_Numero	=		Long(istr_mant.argumento[3])
li_Cliente		=		Integer(istr_mant.argumento[4])
ldt_FechaProc	=	Date(istr_mant.argumento[5])

istr_Mant.Argumento[13] = string(li_Cliente	)

DO
	
	IF Tab_1.Tp_1.dw_3.Retrieve(li_planta, li_cliente, li_especie, ldt_FechaProc, ll_numero, 4) = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		pb_imprimir.Enabled			=	True
		dw_2.Object.ppre_fecpro[1]	=	ldt_FechaProc
		il_Fila							=	1
		Tab_1.Tp_1.SetFocus()
		Tab_1.Tp_1.dw_3.SetFilter("emba_codigo = '" + istr_mant.argumento[7] +"'")
		Tab_1.Tp_1.dw_3.Filter()
		
		FOR li_fila = 1 TO Tab_1.Tp_1.dw_3.RowCount()
			Tab_1.Tp_1.dw_3.Object.etiq_codigo[li_fila]	=	Integer(istr_mant.argumento[14])
			Tab_1.Tp_1.dw_3.Object.cate_codigo[li_fila]	=	Integer(istr_mant.argumento[15])
		NEXT
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

//IF Not istr_mant.Solo_Consulta THEN
//	CHOOSE CASE wf_modifica()
//		CASE -1
//			ib_ok = False
//		CASE 0
//			ll_modif1	=	dw_1.GetNextModified(0, Primary!)
//			ll_modif2	=	dw_2.GetNextModified(0, Primary!)
//		
////			IF ib_ModEncab OR dw_1.GetNextModified(0, Primary!) > 0 THEN
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

dw_1.Reset()
dw_7.Reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.SetFocus()

istr_Mant.Argumento[1]		=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]		=	String(gi_Codespecie)
istr_Mant.Argumento[3]		=  String(Today(),'dd/mm/yyyy')
dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
end event

event ue_imprimir;
CHOOSE CASE Tab_1.SelectedTab
	CASE 1
		ciclo_compacto()
		
	CASE 2
		imprime_dw(Tab_1.tp_2.dw_4)
		
	CASE 3
		imprime_dw(Tab_1.tp_3.dw_5)		
		
	CASE 4
		imprime_dw(Tab_1.tp_4.dw_6)
END CHOOSE
end event

event closequery;//
end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
end event

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0, &
			li_Ancho = 300, li_Alto = 245, li_Siguiente = 255
			
IF dw_8.width > Tab_1.width THEN
	maximo		=	dw_8.width
ELSE
	Tab_1.width	=	This.WorkSpaceWidth() - 400
	maximo		=	Tab_1.width
END IF

Tab_1.x					= 37 + Round((maximo - Tab_1.width) / 2, 0)
Tab_1.y					= 37

gb_3.x					= 37 + Round((maximo - gb_3.width) / 2, 0)
gb_3.y					= 32 + Tab_1.Height

hpb_impresion.x		= 37 + Round((maximo - hpb_impresion.width) / 2, 0)
hpb_impresion.y		= gb_3.y	+	60

dw_8.x					= 37 + Round((maximo - dw_8.width) / 2, 0)
dw_8.y					= Tab_1.Height + gb_3.Height + 64 
dw_8.height				= This.WorkSpaceHeight() - dw_8.y - 41

li_posic_x				= This.WorkSpaceWidth() - 370
li_posic_y				= 350

IF pb_buscar.Visible THEN
	pb_buscar.x				=	li_posic_x
	pb_buscar.y				=	li_posic_y
	pb_buscar.width		=	li_Ancho
	pb_buscar.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				=	li_posic_x
	pb_nuevo.y				=	li_posic_y
	pb_nuevo.width		=	li_Ancho
	pb_nuevo.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF	pb_eliminar.Visible THEN
	pb_eliminar.x			=	li_posic_x
	pb_eliminar.y			=	li_posic_y
	pb_eliminar.width		=	li_Ancho
	pb_eliminar.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x				=	li_posic_x
	pb_grabar.y				=	li_posic_y
	pb_grabar.width		=	li_Ancho
	pb_grabar.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			=	li_posic_x
	pb_imprimir.y			=	li_posic_y
	pb_imprimir.width		=	li_Ancho
	pb_imprimir.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_salir.Visible THEN
	pb_salir.x				=	li_posic_x
	pb_salir.y				=	li_posic_y
	pb_salir.width			=	li_Ancho
	pb_salir.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

pb_eli_det.x				=	li_posic_x
pb_eli_det.y				=	dw_1.y + dw_1.Height - li_Siguiente
pb_eli_det.width		=	li_Ancho
pb_eli_det.height		=	li_Alto

pb_ins_det.x			=	li_posic_x
pb_ins_det.y			=	pb_eli_det.y - li_Siguiente - 10
pb_ins_det.width		=	li_Ancho
pb_ins_det.height		=	li_Alto

Tab_1.Tp_1.dw_camaraysdpsag.Visible	=	False
Tab_1.Tp_1.dw_3.Height					=	1108
Tab_1.Tp_1.dw_3.Y							=	16


end event

type dw_1 from w_mant_encab_deta`dw_1 within w_info_etiqetas_compactos_cajasprod
boolean visible = false
integer x = 4110
integer y = 564
integer width = 3150
integer height = 1444
boolean enabled = false
boolean titlebar = false
string title = ""
string dataobject = "dw_mant_mues_spro_cajasprod_cajasgranel"
boolean hscrollbar = false
boolean vscrollbar = false
boolean livescroll = false
end type

type dw_2 from w_mant_encab_deta`dw_2 within w_info_etiqetas_compactos_cajasprod
boolean visible = false
integer x = 128
integer y = 28
integer width = 3173
integer height = 288
string dataobject = "dw_sel_prograproceso_enca"
end type

event dw_2::itemchanged;call super::itemchanged;Integer	li_Nula
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
		END IF
		istr_mant.Argumento[13] = data

	
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
	
END CHOOSE
//
//HabilitaIngreso(ls_Columna)
end event

type pb_nuevo from w_mant_encab_deta`pb_nuevo within w_info_etiqetas_compactos_cajasprod
boolean visible = false
integer x = 3611
integer y = 376
boolean enabled = false
end type

type pb_eliminar from w_mant_encab_deta`pb_eliminar within w_info_etiqetas_compactos_cajasprod
boolean visible = false
integer x = 3611
integer y = 556
end type

type pb_grabar from w_mant_encab_deta`pb_grabar within w_info_etiqetas_compactos_cajasprod
boolean visible = false
integer x = 3611
integer y = 740
end type

type pb_imprimir from w_mant_encab_deta`pb_imprimir within w_info_etiqetas_compactos_cajasprod
integer x = 3611
integer y = 900
end type

type pb_salir from w_mant_encab_deta`pb_salir within w_info_etiqetas_compactos_cajasprod
integer x = 3611
integer y = 1096
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
end type

type pb_ins_det from w_mant_encab_deta`pb_ins_det within w_info_etiqetas_compactos_cajasprod
boolean visible = false
integer x = 3611
end type

type pb_eli_det from w_mant_encab_deta`pb_eli_det within w_info_etiqetas_compactos_cajasprod
boolean visible = false
integer x = 3611
end type

type pb_buscar from w_mant_encab_deta`pb_buscar within w_info_etiqetas_compactos_cajasprod
boolean visible = false
integer x = 3611
integer y = 196
boolean enabled = false
end type

type tab_1 from tab within w_info_etiqetas_compactos_cajasprod
integer x = 91
integer y = 16
integer width = 3502
integer height = 1260
integer taborder = 30
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
integer width = 3465
integer height = 1132
long backcolor = 16711680
string text = "Compactos"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
long picturemaskcolor = 536870912
dw_camaraysdpsag dw_camaraysdpsag
dw_3 dw_3
end type

on tp_1.create
this.dw_camaraysdpsag=create dw_camaraysdpsag
this.dw_3=create dw_3
this.Control[]={this.dw_camaraysdpsag,&
this.dw_3}
end on

on tp_1.destroy
destroy(this.dw_camaraysdpsag)
destroy(this.dw_3)
end on

type dw_camaraysdpsag from datawindow within tp_1
integer x = 727
integer y = 4
integer width = 1755
integer height = 264
integer taborder = 21
string title = "none"
string dataobject = "dw_mues_seleccion_camaraysdpsag"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within tp_1
integer x = 23
integer y = 316
integer width = 3429
integer height = 808
integer taborder = 20
string title = "none"
string dataobject = "dw_compactos"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;This.AcceptText()
end event

type tp_2 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 112
integer width = 3465
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
integer x = 1170
integer y = 48
integer width = 1161
integer height = 1040
integer taborder = 11
string dataobject = "dw_mues_lotes"
end type

type tp_3 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 112
integer width = 3465
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
integer x = 1275
integer y = 48
integer width = 951
integer height = 1040
integer taborder = 11
string dataobject = "dw_mues_embalajes_etiq"
end type

type tp_4 from userobject within tab_1
boolean visible = false
integer x = 18
integer y = 112
integer width = 3465
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
integer x = 1275
integer y = 48
integer width = 955
integer height = 1040
integer taborder = 21
string dataobject = "dw_mues_calibres_etiq"
end type

type dw_7 from datawindow within w_info_etiqetas_compactos_cajasprod
boolean visible = false
integer x = 4183
integer y = 1584
integer width = 1198
integer height = 864
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_spro_cajasprod_gtin14"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_8 from datawindow within w_info_etiqetas_compactos_cajasprod
event uo_nomover pbm_syscommand
integer x = 78
integer y = 1468
integer width = 3735
integer height = 536
integer taborder = 20
boolean bringtotop = true
boolean titlebar = true
string title = "Detalle Cajas Lote "
string dataobject = "dw_mues_palletfruta_proceso_cajasprod"
boolean vscrollbar = true
boolean border = false
end type

event uo_nomover;uint wParam, lParam

wParam = Message.WordParm

Choose Case wParam
	Case 61456, 61458
	     Message.Processed = True
	     Message.ReturnValue = 0

End Choose
end event

type dw_crea_caja from datawindow within w_info_etiqetas_compactos_cajasprod
boolean visible = false
integer x = 4055
integer y = 2236
integer width = 1477
integer height = 244
integer taborder = 30
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dw_mant_creacion_cajas_granel"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type hpb_impresion from hprogressbar within w_info_etiqetas_compactos_cajasprod
boolean visible = false
integer x = 105
integer y = 1364
integer width = 3360
integer height = 68
boolean bringtotop = true
unsignedinteger maxposition = 100
unsignedinteger position = 20
integer setstep = 1
boolean smoothscroll = true
end type

type gb_3 from groupbox within w_info_etiqetas_compactos_cajasprod
integer x = 78
integer y = 1296
integer width = 3415
integer height = 156
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 30586022
string text = "Progreso Creación de Cajas"
end type

