$PBExportHeader$w_mant_despemba_packing.srw
forward
global type w_mant_despemba_packing from w_mant_encab_deta_csd
end type
type dw_5 from datawindow within w_mant_despemba_packing
end type
type dw_3 from datawindow within w_mant_despemba_packing
end type
type dw_4 from datawindow within w_mant_despemba_packing
end type
type cb_guia from commandbutton within w_mant_despemba_packing
end type
end forward

global type w_mant_despemba_packing from w_mant_encab_deta_csd
integer width = 3639
integer height = 2292
string menuname = ""
event ue_imprimir2 ( )
dw_5 dw_5
dw_3 dw_3
dw_4 dw_4
cb_guia cb_guia
end type
global w_mant_despemba_packing w_mant_despemba_packing

type variables
w_mant_despemba_packing_deta			w_mantencion

uo_Patente			iuo_patente
uo_UsuarPlanta		iuo_UsuaPlta
uo_ClientesProd		iuo_Cliente
uo_guiadespacho	iuo_Guia

Datastore			ids_CorrelMovim

Boolean				ib_existe_folioD = False, ib_Anulada = False
Long					il_NroDespa
String					is_rutchofer
end variables

forward prototypes
public subroutine habilitaingreso (string columna)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine habilitaencab (boolean habilita)
public subroutine wf_envia_mail ()
public function string wf_rescatacorreo (integer ai_planta)
public function boolean wf_existefolio (string as_columna, string as_valor)
public function string wf_buscaformato (long al_planta)
public function long wf_busnuevofoliodespa (integer ai_cliente, integer ai_planta)
end prototypes

event ue_imprimir2();Integer	li_planta, li_cliente, respuesta
Long		ll_despacho, ll_fila_e, ll_fila_d
String		ls_formato

SetPointer(HourGlass!)
dw_2.AcceptText()

li_planta 		=	dw_2.Object.plde_codigo[1]
li_cliente		=	dw_2.Object.clie_codigo[1]
ll_despacho	=	dw_2.Object.defe_numero[1]
//ll_despacho	=	dw_2.Object.defe_guides[1]

Long		fila
str_info	lstr_info

lstr_info.titulo	= "GUIA DE DESPACHO A FRIGORIFICO"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
ls_formato	=	wf_BuscaFormato(li_planta)

If ls_formato <>'' Or Not IsNull(ls_Formato)Then
	vinf.dw_1.DataObject = ls_formato
Else	
	vinf.dw_1.DataObject = "dw_info_guia_despacho_frut"
End If

vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(li_cliente, li_planta, ll_despacho,1,dw_2.Object.defe_guides[1])

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	
	UPDATE dbo.spro_correldoctos
		SET corr_ultdoc = :ll_despacho
	WHERE	plde_codigo	=	:li_planta
		AND	ubdo_codigo	=	:iuo_UsuaPlta.Ubicacion
		AND	tdop_codigo		=	2 
	USING SQLCA;
	
	If sqlca.SQLCode <> 1 Then
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Correlativos")
	Else
		MessageBox("Error","Correlativo de Guia de Despacho actualizado")
	End If
End If

SetPointer(Arrow!)
end event

public subroutine habilitaingreso (string columna);Date	ld_fecha
Boolean	lb_estado = True

dw_2.AcceptText()

If dw_2.RowCount() > 0 Then
	//IsNull(dw_2.Object.defe_numero[1]) OR dw_2.Object.defe_numero[1] = 0 OR &
	If IsNull(dw_2.Object.plde_codigo[1]) Or dw_2.Object.plde_codigo[1] = 0 Or &		
		IsNull(dw_2.Object.defe_fecdes[1]) Or dw_2.Object.defe_fecdes[1] = ld_fecha Or &
		IsNull(dw_2.Object.defe_cancaj[1]) Or dw_2.Object.defe_cancaj[1] = 0 Or &
		IsNull(dw_2.Object.defe_cantar[1]) Or dw_2.Object.defe_cantar[1] = 0 Then

		lb_estado = False
	End If
End If

pb_grabar.Enabled	=	lb_estado
pb_ins_det.Enabled  	= 	lb_estado
pb_eli_det.Enabled  	= 	lb_estado
pb_imprimir.Enabled 	= 	lb_estado
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
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
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				Rollback;
			ELSE
				wf_Envia_Mail()
				IF dw_3.Update(True, False) = 1 THEN		
					Commit;
					lb_Retorno	=	True
					dw_1.ResetUpdate()
					dw_2.ResetUpdate()
					dw_3.ResetUpdate()
				ELSE
					F_ErrorBaseDatos(sqlca, This.Title)
					RollBack;
					lb_Retorno	=	False
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
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine habilitaencab (boolean habilita);If Habilita Then
	dw_2.Object.clie_codigo.Protect	= 0
	dw_2.Object.plde_codigo.Protect 	= 0
	dw_2.Object.defe_numero.Protect	= 0
	dw_2.Object.defe_fecdes.Protect 	= 0
	dw_2.Object.defe_horade.Protect 	= 0
	
	dw_2.Object.defe_numero.Color	=	0
	dw_2.Object.clie_codigo.Color 		=	0
	dw_2.Object.plde_codigo.Color 	=	0
	dw_2.Object.defe_fecdes.Color		= 	0
	dw_2.Object.defe_horade.Color 	= 	0
	
	dw_2.Object.defe_numero.BackGround.Color	=	Rgb(255,255,255)
	dw_2.Object.clie_codigo.BackGround.Color 		=	Rgb(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color 	=	Rgb(255,255,255)
	dw_2.Object.defe_fecdes.BackGround.Color		= 	Rgb(255,255,255)
	dw_2.Object.defe_horade.BackGround.Color 	= 	Rgb(255,255,255)
	
	dw_2.SetColumn("defe_numero")
	dw_2.SetFocus()
Else
	dw_2.Object.defe_numero.Protect	= 1
	dw_2.Object.clie_codigo.Protect	= 1
	dw_2.Object.plde_codigo.Protect	= 1
	dw_2.Object.defe_fecdes.Protect	= 1
	dw_2.Object.defe_horade.Protect	= 1
	
	dw_2.Object.defe_numero.Color	=	Rgb(255,255,255)
	dw_2.Object.clie_codigo.Color 		=	Rgb(255,255,255)
	dw_2.Object.plde_codigo.Color 	=	Rgb(255,255,255)
	dw_2.Object.defe_fecdes.Color		= 	Rgb(255,255,255)
	dw_2.Object.defe_horade.Color 	= 	Rgb(255,255,255)
	
	dw_2.Object.defe_numero.BackGround.Color	=	553648127
	dw_2.Object.clie_codigo.BackGround.Color 		=	553648127
	dw_2.Object.plde_codigo.BackGround.Color 	=	553648127
	dw_2.Object.defe_fecdes.BackGround.Color 	=	553648127
	dw_2.Object.defe_horade.BackGround.Color 	=	553648127
End If
end subroutine

public subroutine wf_envia_mail ();String			ls_DirectorioAct, ls_rut, ls_Contenedor, ls_Archivo,  ls_ArchivoP, ls_embarques, ls_ruta
Long			ll_Fila, ll_Archivo, ll_guia
Boolean		lb_Existe
str_parms	lstr_parms

If IsNull(dw_2.Object.defe_guides[1]) Then 
	MessageBox("Error", "No se genero numero de guia, ingreso y trate de nuevo.", StopSign!, OK!)
	Return
End If

If dw_4.retrieve(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_2.Object.defe_numero[1]) < 1 Then
	Messagebox("Error", "Los pallets involucrados en este despacho no poseen detalle de cajas~n~r" + &
								"No se pudo concretar envio Automático de E-Mail", Exclamation!)
Else
	dw_5.retrieve(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_2.Object.defe_numero[1])
	
	RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
	ls_Archivo	= '\CajasProd' + String(dw_2.Object.plde_codigo[1], '00000') + String(dw_2.Object.defe_guides[1], '00000000') + '.CSV'
	ls_ArchivoP	= '\CajasProd' + String(dw_2.Object.plde_codigo[1], '00000') + String(dw_2.Object.defe_guides[1], '00000000') + '_guia.CSV'
	
	
	If dw_4.SaveAs(ls_ruta + ls_archivo, CSV!, FALSE) = -1 Then
		MessageBox("Error", "No se logro Almacenar el archivo generado en la carpeta especificada~n~r" + &
						ls_ruta + ls_archivo+"~n~r" + "No se pudo concretar envio Automático de E-Mail", StopSign!)
		Return
	Else
	 	dw_5.SaveAs(ls_ruta + ls_archivoP, CSV!, True)
		ChangeDirectory ( ls_ruta )
		
		lstr_parms.string_arg[1]		=	Right(ls_Archivo, Len(ls_Archivo) - 1)
		lstr_parms.string_arg[2]		=	String(dw_2.Object.defe_numero[1])
		lstr_parms.string_arg[3]		=  wf_RescataCorreo(dw_2.Object.defe_plades[1])
		lstr_parms.string_arg[4]		=	ls_ruta + ls_Archivo
		
		OpenWithParm(w_correo_zonas, lstr_parms)
		
		ChangeDirectory ( ls_ruta )
	End If
End If
end subroutine

public function string wf_rescatacorreo (integer ai_planta); String ls_correo
 
 SELECT plde_correo 
    INTO :ls_correo  
    FROM dbo.plantadesp 
   WHERE plde_codigo = :ai_planta
	USING sqlca;
	
IF sqlca.SQLCode =-1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Plantas")
ELSEIF sqlca.SQLCode = 100 THEN
	//RETURN True
ELSE
	RETURN ls_correo
END IF
end function

public function boolean wf_existefolio (string as_columna, string as_valor);Integer	li_planta, li_existe, li_cliente
Long		ll_nfolio

li_cliente	=	dw_2.Object.clie_codigo[1]
li_planta	=	dw_2.Object.plde_codigo[1]
ll_nfolio 	=	dw_2.Object.defe_numero[1]

CHOOSE CASE as_columna
	CASE "plde_codigo"
		li_planta	=	Integer(as_valor)
		
	CASE "defe_numero"
		ll_nfolio 	=	Long(as_valor)
		
END CHOOSE

SELECT	Count(*)
	INTO	:li_existe
	FROM	dbo.DESPAFRIGOEN
	WHERE	plde_codigo	=	:li_planta
	AND	defe_numero	=	:ll_nfolio 
	And   clie_codigo =  :li_cliente 
	Using SQLCA;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Despafrigoen")
	RETURN False
ELSEIF li_existe > 0 THEN
	istr_mant.argumento[1]	= String(li_planta)
	istr_mant.argumento[2]	= String(ll_nfolio)
	ib_existe_folioD				=	TRUE
	This.TriggerEvent("ue_recuperadatos")

	istr_mant.argumento[3]	= 	String(dw_2.Object.clie_codigo[1])
	istr_mant.argumento[4]	= 	String(dw_2.Object.defe_cantar[1])
	istr_mant.argumento[8]	=	String(dw_2.Object.defe_cancaj[1])
   RETURN False
ELSE
	IF IsNull(ll_nfolio) THEN
		istr_mant.argumento[1]	= String(li_planta)
		istr_mant.argumento[2]	= String(ll_nfolio)
		RETURN False
	ELSE
	    MessageBox("Atención","Número de Documento No ha sido generado. Ingrese Otro.")
	   RETURN True
	END IF	
END IF





end function

public function string wf_buscaformato (long al_planta);string ls_existe

SELECT prpa_ftogdd
	INTO	:ls_existe
	FROM	dbo.spro_paramplanta
	Where plde_codigo = :al_Planta
	USING sqlca;
				
If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla spro_paramplanta")
	SetNull(ls_existe)	
Else
	SetNull(ls_existe)
End If

Return ls_existe
end function

public function long wf_busnuevofoliodespa (integer ai_cliente, integer ai_planta);/* Busca Folio para hacer un  Despacho de pallet */

Integer	li_Planta
Long		ll_Numero, ll_Inicia, ll_Termin, ll_Actual, ll_Quedan, ll_despacho
Boolean	lb_Nulo

li_Planta		=	ai_planta
ll_Numero	=	0

Select IsNull(Max(defe_numero), 0) + 1
	Into :ll_Numero
    From dbo.despafrigoen
   Where plde_codigo = :ai_planta
     And clie_codigo = :ai_Cliente
Using SQLCA;
		
If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla Despafrigoen")
	ll_Numero = 0
	Return ll_Numero
End If

//ids_CorrelMovim.Retrieve(li_Planta,2)
//If ids_CorrelMovim.RowCount() > 0 Then
//	ll_Inicia		=	ids_CorrelMovim.Object.como_inicia[1]
//	ll_Termin	=	ids_CorrelMovim.Object.como_termin[1]
//	ll_Actual		=	ids_CorrelMovim.Object.como_actual[1]
//	
//	If Isnull(ll_Inicia) Then ll_Inicia	=	0
//	If Isnull(ll_Termin) Then ll_Termin	=	0
//	If Isnull(ll_Actual) Then ll_Actual	=	0
//		
//	If Isnull(ll_despacho) OR String(ll_despacho) = '' OR ll_despacho < ll_Inicia Then
//		ll_Actual = ll_Inicia
//	Else
//		ll_Actual=	ll_despacho
//	End If	
//
//	If ll_Inicia >= 0 AND ll_Termin > 0 Then
//		If ll_Actual	=	0	Then
//			ll_Actual	=	ll_Inicia + 1
//		Else
//			ll_Actual++		
//		End If
//		
//		If ll_Actual >= ll_Inicia AND	ll_Actual <= ll_Termin	Then
//			If ll_Actual > ll_Termin Then
//				Messagebox("Atención","No Existen Números de Folios Disponibles de Movimiento de Despacho",exclamation!) 			
//				ll_Numero	=	0
//				Return ll_Numero
//			ElseIf ll_Actual = ll_Termin Then
//				Messagebox("Atención","Ultimo Número de Folio Disponible de Movimiento de Despacho",exclamation!) 			
//			End If
//			
//			ll_Numero	=	ll_Actual	
//			ll_Quedan	=	(ll_Termin - ll_Actual)
//			
//			If ll_Quedan <= 3 Then
//				Messagebox("Atención","Existen "+String(ll_Quedan)+" Número de Folio Disponible de Movimiento de Despacho",exclamation!) 			
//			End If
//			ids_CorrelMovim.Object.como_actual[1]	=	ll_Actual
//		Else
//			Messagebox("Atención","Número de Folio Generado no se encuentra Disponible dentro del rango de Movimiento de Despacho",exclamation!) 
//			ll_Numero	=	0
//			Return ll_Numero
//		End If
//	Else
//		Messagebox("Atención","Número de Folio Generado no se encuentra Disponible dentro del rango de Movimiento de Despacho",exclamation!) 
//		ll_Numero	=	0
//		Return ll_Numero	
//	End If
//Else
//	Messagebox("Atención","No Existe Ningún Número de Movimiento de Despacho",exclamation!)
//	ll_Numero	=	0
//	Return ll_Numero
//End If

Return ll_Numero
end function

on w_mant_despemba_packing.create
int iCurrent
call super::create
this.dw_5=create dw_5
this.dw_3=create dw_3
this.dw_4=create dw_4
this.cb_guia=create cb_guia
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_5
this.Control[iCurrent+2]=this.dw_3
this.Control[iCurrent+3]=this.dw_4
this.Control[iCurrent+4]=this.cb_guia
end on

on w_mant_despemba_packing.destroy
call super::destroy
destroy(this.dw_5)
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.cb_guia)
end on

event open;call super::open;dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)

ids_CorrelMovim	=	Create DataStore
iuo_UsuaPlta		=	Create uo_usuarplanta
iuo_Cliente			=	Create uo_ClientesProd	
iuo_Guia				=	Create uo_GuiaDespacho

iuo_UsuaPlta.Existe(gstr_us.Nombre, gstr_ParamPlanta.CodigoPlanta, False, sqlca)

ids_CorrelMovim.DataObject		=	'dw_mues_correlmoviemientos_despa'
ids_CorrelMovim.SetTransObject(sqlca)

istr_mant.dw						=	dw_3

This.Height	=	2520
end event

event ue_nuevo;call super::ue_nuevo;dw_2.Object.defe_tiposa[1]	=	22
dw_2.Object.defe_guiaem[1] = 0
il_NroDespa = 0
ib_Anulada = False

dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.clie_codigo[1]	=	gi_codexport

iuo_Cliente.Existe(gi_CodExport, False, Sqlca) //Guia_Electronica

If gstr_parEmpresa.PlantaDestino > 0 Then dw_2.Object.defe_plades[1]=	gstr_parEmpresa.PlantaDestino

If gi_Emisor_Electronico = 1 Then
	dw_2.Object.defe_guides.Protect	= 1
	dw_2.Object.defe_guides.Color		= Rgb(255,255,255)
	dw_2.Object.defe_guides.BackGround.Color	= 553648127
Else
	dw_2.Object.defe_guides.Protect	= 0
	dw_2.Object.defe_guides.Color		= 0
	dw_2.Object.defe_guides.BackGround.Color	= Rgb(255,255,255)
End If

dw_3.SetRedraw(False)
dw_3.Reset()
dw_3.SetRedraw(True)

HabilitaEncab(True)
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;Integer			li_filas, li_filaInsertada, li_find
uo_variedades	iuo_variedad

iuo_variedad				=	CREATE uo_variedades

istr_mant.argumento[1]	=	String(dw_2.Object.plde_codigo[1])
istr_mant.argumento[2]	=	String(dw_2.Object.clie_codigo[1])

OpenWithParm(w_mantencion, istr_mant)

FOR li_filas = 1 to dw_3.RowCount()
	
	li_find	=	dw_1.Find("plde_codigo = " + String(dw_3.Object.plde_codigo[li_filas]) + " and " + &
								 "clie_codigo = " + String(dw_3.Object.clie_codigo[li_filas]) + " and " + &
								 "paen_numero = " + String(dw_3.Object.paen_numero[li_filas]), 1, dw_1.RowCount())
	
	IF li_find < 1 THEN
		li_filaInsertada												=	dw_1.InsertRow(0)
		
		dw_1.Object.plde_codigo[li_filaInsertada]	 		= 	dw_3.Object.plde_codigo[li_filas]
		dw_1.Object.clie_codigo[li_filaInsertada] 		= 	dw_3.Object.clie_codigo[li_filas]
		dw_1.Object.paen_numero[li_filaInsertada] 		= 	dw_3.Object.paen_numero[li_filas]
		iuo_variedad.Existe(dw_3.Object.espe_codigo[li_filas], dw_3.Object.vari_codigo[li_filas], false, sqlca)
		dw_1.Object.vari_nombre[li_filaInsertada]			=	iuo_variedad.NombreVariedad
		dw_1.Object.cate_codigo[li_filaInsertada]			=	dw_3.Object.cate_codigo[li_filas]
		dw_1.Object.paen_ccajas[li_filaInsertada]			=	dw_3.Object.paen_ccajas[li_filas]
		dw_1.Object.paen_tipopa[li_filaInsertada]			=	dw_3.Object.paen_tipopa[li_filas]
		dw_1.Object.defe_pcopda[li_filaInsertada]			=	1
		dw_3.Object.paen_estado[li_filas]					=	2
	END IF
	
NEXT
end event

event ue_recuperadatos;call super::ue_recuperadatos;Integer	li_planta, li_cliente, respuesta
Long		ll_despacho, ll_fila_e, ll_fila_d, ll_Null


dw_2.AcceptText()

SetNull(ll_Null)
li_planta 		=	dw_2.Object.plde_codigo[1]
li_cliente		=	dw_2.Object.clie_codigo[1]
il_NroDespa	=	dw_2.Object.defe_numero[1]

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()

	ll_fila_e	= dw_2.Retrieve(li_cliente, li_planta, il_NroDespa)
	
	If ll_fila_e = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	Else
		DO
			ll_fila_d	= dw_1.Retrieve(li_cliente, li_planta, il_NroDespa, 2)

			If ll_fila_d = -1 Then
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
			Else
				pb_imprimir.Enabled	= True
				
				If 	dw_2.Object.defe_estado[1]	=	1	Then
					istr_mant.solo_consulta	=	True
				Else
					istr_mant.solo_consulta 	=	False
					pb_eli_det.Enabled		=	True
					pb_ins_det.Enabled		=	True
					pb_grabar.Enabled		=	True
					pb_eliminar.Enabled		=	True
				End If
								
				If ll_fila_d > 0 Then
					pb_imprimir.Enabled	= True
					
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
					
					If gi_Emisor_Electronico = 1 Then
						If dw_2.Object.defe_guiaem[1] = 1 Then
							If MessageBox('Atencion', 'Esta guia esta emitida, si desea efectuar cambios debe anular.~r~r' + &
									'Desea Anular?', Exclamation!, YesNo!, 2) = 2 Then
								istr_mant.solo_consulta	=	True
								ib_Anulada = True
							Else
								dw_2.Object.defe_guiaem[1] = 0
								dw_2.Object.defe_guides[1] =  ll_Null
							End If
						End If
					End If
					
					If IsNull(dw_2.Object.defe_plades[1]) Then
						If gstr_parEmpresa.PlantaDestino > 0 Then 
							dw_2.Object.defe_plades[1]=	gstr_parEmpresa.PlantaDestino
						End If
					End If
				Else
					If 	dw_2.Object.defe_estado[1]  =	1 Then pb_ins_det.Enabled	=	True							
				End If
			End If
		LOOP WHILE respuesta = 1

		If respuesta = 2 Then Close(This)
	End If
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

If respuesta = 2 Then Close(This)
end event

event ue_antesguardar;call super::ue_antesguardar;Long			ll_nromovto
Integer 		li_filas

If IsNull(dw_2.Object.defe_plades[1]) Or dw_2.Object.defe_plades[1] = 0 Then
	MessageBox("Error de Consistencia", "Falta Ingresar planta de destino.", StopSign!, Ok!)
	dw_2.SetColumn("defe_plades")
	Message.DoubleParm = -1
	Return
End If

If (dw_1.Object.totcajas[1])<> dw_2.Object.defe_cancaj[1]  Then
	MessageBox("Error de Consistencia", "Cajas No Corresponden con Detalle.", StopSign!, Ok!)
	dw_2.SetColumn("defe_cancaj")
	Message.DoubleParm = -1
End If
	
If (dw_1.Object.compute_1[1])<> dw_2.Object.defe_cantar[1]  Then
	MessageBox("Error de Consistencia", "Tarjas No Corresponden con Detalle.", StopSign!, Ok!)
	dw_2.SetColumn("defe_cantar")
	Message.DoubleParm = -1
End If

If il_NroDespa < 1 Then
	il_NroDespa	=	wf_BusNuevoFolioDespa(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1])
End If

If il_NroDespa > 0 Then
	dw_2.Object.defe_numero[1]	= il_NroDespa
	dw_2.SetItem(1, "defe_numero",il_NroDespa)
	
	istr_mant.argumento[2]	= String(il_NroDespa)
	
	FOR li_filas = 1 TO dw_1.RowCount()
		 dw_1.Object.defe_numero[li_filas]	= il_NroDespa
	NEXT		
End If

dw_3.ResetUpdate()

FOR li_filas = 1 TO dw_3.RowCount()
	dw_3.SetItemStatus(li_filas, "paen_estado", Primary!, DataModIfied!)
NEXT

dw_2.Object.defe_horade[1]	=	Time(String(Now(), 'hh:mm:ss'))
end event

event ue_imprimir;Integer	li_planta, li_cliente, respuesta
Long		ll_despacho, ll_fila_e, ll_fila_dl, ll_Guia

SetPointer(HourGlass!)
dw_2.AcceptText()

li_Planta 		=	dw_2.Object.plde_codigo[1]
li_Cliente		=	dw_2.Object.clie_codigo[1]
ll_Despacho	=	dw_2.Object.defe_numero[1]

Long		Fila
str_info	lstr_info

lstr_info.titulo	= "DESPACHO DE FRUTA EMBALADA"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_despafrigoen"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(li_cliente, li_planta, ll_despacho)

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

event ue_seleccion;call super::ue_seleccion;String ls_nula
Str_busqueda	lstr_busq

lstr_busq.argum[1]	= 	String(dw_2.Object.clie_codigo[1])
lstr_busq.argum[2]	= 	String(dw_2.Object.plde_codigo[1])

OpenWithParm(w_busc_despafrigoen, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[5] <> "" THEN
	dw_2.Object.defe_numero[1]	=	Long(lstr_busq.argum[5])
	dw_2.AcceptText()
	TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_borra_detalle;call super::ue_borra_detalle;Integer	li_respuesta, li_fila, li_find

li_respuesta = messagebox("Advertencia", "¿Desea borrar la fila seleccionada?", Question!, YesNo!)

IF li_respuesta = 1 AND dw_1.GetSelectedRow(0) > 0 THEN
	FOR li_fila = 1 TO dw_1.RowCount()
		IF dw_1.IsSelected(li_fila) THEN
			li_find	=	dw_3.Find("plde_codigo = " + String(dw_1.Object.plde_codigo[li_fila]) + " and " + &
								       "clie_codigo = " + String(dw_1.Object.clie_codigo[li_fila]) + " and " + &
								 		 "paen_numero = " + String(dw_1.Object.paen_numero[li_fila]), 1, dw_3.RowCount())
			IF li_find > 0 THEN
				dw_3.DeleteRow(li_find)
				
			END IF
			
			dw_1.DeleteRow(li_fila)
			li_fila = li_fila -1

		END IF
	NEXT
END IF
end event

event resize;call super::resize;
IF cb_guia.Visible THEN
	cb_guia.x			=	pb_salir.x
	cb_guia.y			=	pb_salir.y + 255
	cb_guia.width		=	300
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_mant_despemba_packing
integer x = 78
integer y = 1324
integer width = 2926
integer height = 660
string title = "Detalle del Despacho"
string dataobject = "dw_mues_despafrigode"
end type

event dw_1::losefocus;//
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_mant_despemba_packing
integer x = 37
integer y = 36
integer width = 2894
integer height = 1108
string dataobject = "dw_mant_despafrigoen"
end type

event dw_2::itemchanged;call super::itemchanged;String		ls_columna, ls_null

SetNull(ls_null)
ls_columna = dwo.Name

Choose Case ls_columna
	Case "clie_codigo"
		istr_mant.argumento[3]	= Data
		If Not iuo_Cliente.Existe(Long(Data), True, Sqlca) Then
			dw_2.SetItem(il_fila, "clie_codigo", gi_CodExport)
			Return 1
		Else
			iuo_Cliente.Existe(Long(Data), False, Sqlca)
			If iuo_Cliente.Guia_Electronica = 1 Then 
				This.Object.defe_guides.Protect = 1
				This.Object.defe_guides.BackGround.Color	= 553648127
				This.SetItem(Row, "defe_guides", Long(ls_Null))
			Else
				This.Object.defe_guides.Protect = 0
				This.Object.defe_guides.BackGround.Color	= Rgb(255,255,255)
			End If
		End If
					
	Case "plde_codigo"
		wf_ExisteFolio(ls_columna, data)
		
	Case "defe_numero"
		If wf_ExisteFolio(ls_columna, data) Then
			This.SetItem(1, ls_columna, Integer(ls_null))
			Return 1
		End If
		
	Case "defe_cantar"
		istr_mant.argumento[4]	= data
		
	Case "defe_cancaj"
		istr_mant.argumento[8]	= data

	Case "defe_tiposa"
		istr_mant.argumento[27] = data

	Case "defe_fecdes"
		If Not f_validafechatempo(date(data)) Then
			This.SetItem(Row, ls_Columna, Date(ls_Null))
			Return 1
		End If	
		
	CASE "defe_chfrut"
		is_rutchofer = F_verrut(data, True)
		IF is_rutchofer = "" THEN
			dw_2.SetItem(1, "defe_chfrut", ls_Null)
			RETURN 1
		ELSE
			dw_2.SetItem(1, "defe_chfrut", is_rutchofer)
			RETURN 1
		END IF		

End Choose

HabilitaIngreso(ls_columna)
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_mant_despemba_packing
integer x = 3086
integer y = 268
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_mant_despemba_packing
integer x = 3086
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_mant_despemba_packing
integer x = 3086
integer y = 628
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_mant_despemba_packing
integer x = 3086
integer y = 808
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_mant_despemba_packing
integer x = 3122
integer y = 1040
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_mant_despemba_packing
integer x = 3013
integer y = 1572
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_mant_despemba_packing
integer x = 3013
integer y = 1744
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_mant_despemba_packing
integer x = 3086
end type

type dw_5 from datawindow within w_mant_despemba_packing
boolean visible = false
integer x = 2469
integer y = 1132
integer width = 215
integer height = 132
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_genera_archivo_peppi"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within w_mant_despemba_packing
boolean visible = false
integer x = 155
integer y = 1320
integer width = 2898
integer height = 248
integer taborder = 70
string title = "none"
string dataobject = "dw_mues_palletencab_packing"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_mant_despemba_packing
boolean visible = false
integer x = 2062
integer y = 1084
integer width = 302
integer height = 192
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_arch_planos_cajasprod_despaemba"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type cb_guia from commandbutton within w_mant_despemba_packing
integer x = 3099
integer y = 1328
integer width = 302
integer height = 112
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "GUIA SII"
end type

event clicked;Integer	li_planta, li_cliente, respuesta
Long		ll_despacho, ll_fila_e, ll_fila_dl, ll_Guia, ll_Fila

DataStore 	lds_DataStore

dw_2.AcceptText()

li_Planta 		=	dw_2.Object.plde_codigo[1]
li_Cliente		=	dw_2.Object.clie_codigo[1]
ll_Despacho	=	dw_2.Object.defe_numero[1]


lds_datastore = CREATE datastore
lds_datastore.DataObject = "dw_info_despafrigoen"
lds_datastore.SetTransObject (SQLCA)

ll_fila = lds_datastore.Retrieve(li_cliente, li_planta, ll_despacho)

If ll_fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else

	If Not ib_Anulada Then
		If MessageBox("Guia Despacho", "¿Desea imprimir Guía de Despacho SII?", Question!, YesNo!) = 1 Then
			If gi_Emisor_Electronico = 1 Then
				If dw_2.Object.defe_guiaem[1] = 0 Then 
					ll_Guia = iuo_Guia.of_emiteguia_fruticola(li_Planta, li_Cliente, ll_Despacho)
					If ll_Guia > 0 Then
						If iuo_Guia.of_GeneraLibroGuia(1) Then 
							iuo_Guia.of_RecuperaPDF(ll_Guia,lds_datastore.Object.defe_fecdes[1], 1)
							dw_2.Object.defe_guides[1] = ll_Guia
							dw_2.Object.defe_guiaem[1] = 1
							TriggerEvent('ue_guardar')
							PostEvent("ue_imprimir2")
						Else
							MessageBox('Alerta', 'No se pudo actualziar Libro de guias de despacho.', Information!, OK!)
						End If
					End If
				Else
					iuo_Guia.of_RecuperaPDF(dw_2.Object.defe_guides[1],lds_datastore.Object.defe_fecdes[1], 1)
					PostEvent("ue_imprimir2")
				End If
			Else
				PostEvent("ue_imprimir2")
			End If
		Else
			PostEvent("ue_imprimir2")
		End If
	Else
		PostEvent("ue_imprimir2")
	End If
End If

SetPointer(Arrow!)
end event

