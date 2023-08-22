$PBExportHeader$w_gene_archivo_repalletizacion_repa_sag.srw
$PBExportComments$Genera archivo Plano SAG por Repalletizaciones.
forward
global type w_gene_archivo_repalletizacion_repa_sag from window
end type
type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_repalletizacion_repa_sag
end type
type uo_selplantas from uo_seleccion_plantas within w_gene_archivo_repalletizacion_repa_sag
end type
type cbx_pda from checkbox within w_gene_archivo_repalletizacion_repa_sag
end type
type dw_6 from datawindow within w_gene_archivo_repalletizacion_repa_sag
end type
type dw_5 from datawindow within w_gene_archivo_repalletizacion_repa_sag
end type
type dw_4 from datawindow within w_gene_archivo_repalletizacion_repa_sag
end type
type dw_3 from datawindow within w_gene_archivo_repalletizacion_repa_sag
end type
type em_numero from editmask within w_gene_archivo_repalletizacion_repa_sag
end type
type st_numero from statictext within w_gene_archivo_repalletizacion_repa_sag
end type
type st_4 from statictext within w_gene_archivo_repalletizacion_repa_sag
end type
type st_3 from statictext within w_gene_archivo_repalletizacion_repa_sag
end type
type em_fecha from editmask within w_gene_archivo_repalletizacion_repa_sag
end type
type st_5 from statictext within w_gene_archivo_repalletizacion_repa_sag
end type
type st_1 from statictext within w_gene_archivo_repalletizacion_repa_sag
end type
type pb_salir from picturebutton within w_gene_archivo_repalletizacion_repa_sag
end type
type pb_grabar from picturebutton within w_gene_archivo_repalletizacion_repa_sag
end type
type dw_2 from datawindow within w_gene_archivo_repalletizacion_repa_sag
end type
type dw_1 from datawindow within w_gene_archivo_repalletizacion_repa_sag
end type
type st_8 from statictext within w_gene_archivo_repalletizacion_repa_sag
end type
type sle_mensa from singlelineedit within w_gene_archivo_repalletizacion_repa_sag
end type
type st_6 from statictext within w_gene_archivo_repalletizacion_repa_sag
end type
end forward

global type w_gene_archivo_repalletizacion_repa_sag from window
integer width = 2697
integer height = 1312
boolean titlebar = true
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
string icon = "TABLA.ICO"
event ue_guardar ( )
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
event ue_genera_facturas pbm_custom04
uo_selcliente uo_selcliente
uo_selplantas uo_selplantas
cbx_pda cbx_pda
dw_6 dw_6
dw_5 dw_5
dw_4 dw_4
dw_3 dw_3
em_numero em_numero
st_numero st_numero
st_4 st_4
st_3 st_3
em_fecha em_fecha
st_5 st_5
st_1 st_1
pb_salir pb_salir
pb_grabar pb_grabar
dw_2 dw_2
dw_1 dw_1
st_8 st_8
sle_mensa sle_mensa
st_6 st_6
end type
global w_gene_archivo_repalletizacion_repa_sag w_gene_archivo_repalletizacion_repa_sag

type variables
str_mant			istr_mant
str_busqueda	istr_busq
Boolean			ib_Anulacion
Integer			ii_PlantaSag, ii_CantTarjas, ii_CantTarjasCambio, &
               		ii_CantInspec, ii_CantInspecCambio, ii_nuevoaltura, &
               		ii_CantParticipa,ii_CantParticipaCambio, ii_tiporepa, ii_CantParticipa_2,&
					ii_CantParticipaori, ii_CantTarjasori,ii_CantTarjasdes, ii_CantParticipades,&
					ii_CantInspecori,ii_CantInspecdes
Date				id_FechaRepa, id_FechaAltu, id_FechaAcceso
Time				it_HoraAcceso
Long				il_proceso

DataStore ids_palletencab, ids_inspeccion
end variables

forward prototypes
public function string codigosagespecie (integer ai_especie)
public function integer codigoplantasag (integer planta)
public function string codigodestino (long al_nropallet)
public function string fechainspeccion (long al_nropallet)
public function string buscainspeccion (long al_nropallet)
public function boolean existemovimiento (long ai_numero)
public function boolean palletanulado (long al_nropallet)
end prototypes

event ue_guardar();Integer		li_inspeccion, li_region
Long			ll_Fila, ll_Filas, ll_FilaDet, ll_Numero, ll_Pallet,&
				ll_NroSag, ll_fila2, ll_filas2, ll_fila3, ll_filas3, ll_PalletNvo,&
				ll_PalletAnguo, ll_Filasdw_2, ll_Filasdw_4, ll_Filas_eli, ll_Filas_elim,&
				ll_PalletAnguo_alt, ll_numero2, ll_contador, ll_contador2, ll_contador3
String			ls_Archivo, ls_Registro, ls_NumeroSAG, ls_Destino
Date			ld_FecProc

dw_3.reset()
dw_2.reset()
dw_1.reset()
ids_inspeccion.reset()
ids_palletencab.reset()

If Not DirectoryExists(gs_disco+":\GeneradosSAG") Then
	MessageBox('Error', 'El directorio para la generacion de archivo: ' + gs_disco+":\GeneradosSAG" + &
					"~n~nNo existe, favor revisar o comunicarse con informatica, para activar directorio.", StopSign!, Ok!)
	Return
End If

dw_2.SetTransObject(Sqlca)
dw_3.SetTransObject(Sqlca)
ids_inspeccion.SetTransObject(Sqlca)
ids_palletencab.SetTransObject(Sqlca)

ii_PlantaSag		=	CodigoPlantaSag(uo_SelPlantas.Codigo)
ls_NumeroSAG	=	String(Long(em_numero.Text), '00000')
ll_NroSag		=	Long(ls_NumeroSAG)
ld_FecProc		=	Today()

If ii_tiporepa = 3  OR ii_tiporepa = 7 Then
	SELECT count(distinct red.paen_numero)
	INTO :ll_contador
	FROM dbo.repalletdeta as red, dbo.repalletenca as ren, dbo.palletencab as pen
	WHERE red.clie_codigo = ren.clie_codigo 
	AND	red.plde_codigo = ren.plde_codigo
	AND	red.repe_numero = ren.repe_numero
	AND   red.clie_codigo = pen.clie_codigo 
	AND	red.plde_codigo = pen.plde_codigo
	AND	red.paen_numero = pen.paen_numero
	AND   pen.paen_inspec > 0 
	AND	ren.repe_nrosag = :ll_nrosag
	AND	ren.clie_codigo = :uo_SelCliente.Codigo
	AND	ren.plde_codigo = :uo_SelPlantas.Codigo;
	
	SELECT count(distinct paen_nroori)
	INTO :ll_contador2
	FROM dbo.repalletdeta as red, dbo.repalletenca as ren, dbo.palletencab as pen
	WHERE red.clie_codigo = ren.clie_codigo 
	AND	red.plde_codigo = ren.plde_codigo
	AND	red.repe_numero = ren.repe_numero
	AND   red.clie_codigo = pen.clie_codigo 
	AND	red.plde_codigo = pen.plde_codigo
	AND	red.paen_nroori = pen.paen_numero
	AND   pen.paen_inspec > 0 
	AND	ren.repe_nrosag = :ll_nrosag
	AND	ren.clie_codigo = :uo_SelCliente.Codigo
	AND	ren.plde_codigo = :uo_SelPlantas.Codigo;
Else
	SELECT count(distinct red.paen_numero)
	INTO :ll_contador
	FROM dbo.repalletdeta as red, dbo.repalletenca as ren, dbo.palletencab as pen
	WHERE red.clie_codigo = ren.clie_codigo 
	AND	red.plde_codigo = ren.plde_codigo
	AND	red.repe_numero = ren.repe_numero
	AND   red.clie_codigo = pen.clie_codigo 
	AND	red.plde_codigo = pen.plde_codigo
	AND	red.paen_numero = pen.paen_numero
	AND   pen.paen_inspec > 0 	
	AND	ren.repe_nrosag = :ll_nrosag
	AND	ren.clie_codigo = :uo_SelCliente.Codigo
	AND	ren.plde_codigo = :uo_SelPlantas.Codigo;
	
	SELECT count(distinct paen_nroori)
	INTO :ll_contador2
	FROM dbo.repalletdeta as red, dbo.repalletenca as ren, dbo.palletencab as pen
	WHERE red.clie_codigo = ren.clie_codigo 
	AND	red.plde_codigo = ren.plde_codigo
	AND	red.repe_numero = ren.repe_numero
	AND   red.clie_codigo = pen.clie_codigo 
	AND	red.plde_codigo = pen.plde_codigo
	AND	red.paen_nroori = pen.paen_numero
	AND   pen.paen_inspec > 0 
	AND	ren.repe_nrosag = :ll_nrosag
	AND	ren.clie_codigo = :uo_SelCliente.Codigo
	AND	ren.plde_codigo = :uo_SelPlantas.Codigo;
	
	SELECT count(distinct paen_nroori)
	INTO :ll_contador3
	FROM dbo.repalletdeta as red, dbo.repalletenca as ren, dbo.palletencab as pen
	WHERE red.clie_codigo = ren.clie_codigo 
	AND	red.plde_codigo = ren.plde_codigo
	AND	red.repe_numero = ren.repe_numero
	AND   red.clie_codigo = pen.clie_codigo 
	AND	red.plde_codigo = pen.plde_codigo
	AND	red.paen_nroori = pen.paen_numero
	AND   pen.paen_inspec > 0 
	AND	ren.repe_nrosag = :ll_nrosag
	AND	ren.clie_codigo = :uo_SelCliente.Codigo
	AND	ren.plde_codigo = :uo_SelPlantas.Codigo
	AND	red.repd_totcao = 0;
End If	
	
If ib_Anulacion Then
		
Else
	If li_region <> 4 Then
		ls_Archivo	=	String(ii_PlantaSag, '000') + ls_NumeroSAG + ".REP"
	Else
		ls_Archivo	=	String(ii_PlantaSag, '0000') + ls_NumeroSAG + ".REP"
	End If		
	
	ls_Registro	=	String(ii_PlantaSag, '0000') 
	ls_Registro	+=	String(Long(em_numero.Text), '0000')
	ll_FilaDet		=	dw_1.InsertRow(0)
					
	dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
	
	If Long(istr_mant.argumento[3]) > 0 Then	
		ll_Filas		= dw_2.Retrieve(uo_SelPlantas.Codigo, Long(istr_mant.argumento[3]), 0)
		ll_Filas_eli	= dw_5.Retrieve(uo_SelPlantas.Codigo, Long(istr_mant.argumento[3]), 0)
		ll_PalletNvo 	= dw_4.Retrieve(uo_SelPlantas.Codigo, Long(istr_mant.argumento[3]), 0,ii_tiporepa)									 
				
		If ll_Filas = -1 Then
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Pallets")
		ElseIf ll_Filas = 0 Then
			MessageBox("Atención", "No hay información con Repalletizaje indicado.~r~r" + &
						  "Ingrese otra Operación.", Exclamation!, Ok!)
			pb_grabar.Enabled	= False
			em_numero.SetFocus()
		Else
			If ii_tiporepa = 3  OR ii_tiporepa = 7 Then
				FOR ll_Fila = 1 TO ll_PalletNvo
					If dw_4.Object.repd_tipood[ll_Fila] = 1 Then
						If palletanulado(dw_4.Object.paen_numero[ll_Fila]) Then
							ls_Registro	=	"M"
							ls_Registro	+=	BuscaInspeccion(dw_4.Object.paen_numero[ll_Fila])
							ls_Registro	+=	uo_SelCliente.ClienteRotulado
							ls_Registro	+=	String(dw_4.Object.paen_numero[ll_Fila], '0000000')
							ls_Registro	+=	String(dw_4.Object.pafr_ccajas[ll_Fila], '0000')
							ls_Registro	+=	CodigoSagEspecie(dw_4.Object.espe_codigo[ll_Fila])
							ls_Destino  =  String(dw_4.Object.dest_codigo[ll_Fila], '000')
							ls_Registro	+=	ls_Destino
							ls_Registro	+=	FechaInspeccion(dw_4.Object.paen_numero[ll_Fila])
						
							ll_FilaDet	=	dw_1.InsertRow(0)							
							dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
							ll_Pallet = dw_4.Object.paen_numero[ll_Fila]
							
							//	Actualiza Inspección del Pallet Nuevo
							ll_fila3 = ids_palletencab.Retrieve(uo_SelCliente.Codigo,uo_SelPlantas.Codigo,ll_Pallet)
							If ll_fila3 > 0 Then
								FOR ll_filas3 = 1 TO ll_Fila3
									ids_palletencab.Object.paen_inspec[ll_filas3] = 1
									ids_palletencab.Object.dest_codigo[ll_filas3] = Integer(ls_Destino)
								NEXT	
							
								If ids_palletencab.Update() = 1 Then 
									Commit;
									If sqlca.SQLCode <> 0 Then
										F_ErrorBaseDatos(sqlca, This.Title)
									Else
										ids_palletencab.ResetUpdate()
									End If
								Else
									F_ErrorBaseDatos(sqlca, This.Title)
									RollBack;										
								End If	
							End If
						End If	
					Else
						If palletanulado(dw_4.Object.paen_numero[ll_Fila]) Then
							ls_Registro	=	"A"
							ls_Registro	+=	BuscaInspeccion(dw_4.Object.paen_numero[ll_Fila])
							ls_Registro	+=	uo_SelCliente.ClienteRotulado
							ls_Registro	+=	String(dw_4.Object.paen_numero[ll_Fila], '0000000')
							ls_Registro	+=	String(dw_4.Object.pafr_ccajas[ll_Fila], '0000')
							ls_Registro	+=	CodigoSagEspecie(dw_4.Object.espe_codigo[ll_Fila])
							ls_Destino  =  String(dw_4.Object.dest_codigo[ll_Fila], '000')
							ls_Registro	+=	ls_Destino
							ls_Registro	+=	FechaInspeccion(dw_4.Object.paen_numero[ll_Fila])
							
							ll_FilaDet	=	dw_1.InsertRow(0)
							dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
							
							ll_Pallet = dw_4.Object.paen_numero[ll_Fila]
							//	Actualiza Inspección del Pallet Nuevo
							ll_fila3 = ids_palletencab.Retrieve(uo_SelCliente.Codigo,uo_SelPlantas.Codigo,ll_Pallet)
							
							If ll_fila3 > 0 Then
								FOR ll_filas3 = 1 TO ll_Fila3
									ids_palletencab.Object.paen_inspec[ll_filas3] = 1
									ids_palletencab.Object.dest_codigo[ll_filas3] = Integer(ls_Destino)
								NEXT	
							
								If ids_palletencab.Update() = 1 Then 
									Commit;
									If sqlca.SQLCode <> 0 Then
										F_ErrorBaseDatos(sqlca, This.Title)
									Else
										ids_palletencab.ResetUpdate()
									End If
								Else
									F_ErrorBaseDatos(sqlca, This.Title)
									RollBack;										
								End If	
							End If
						End If		
					End If
				NEXT	
				
				FOR ll_PalletAnguo = 1 TO ll_Filas_eli
					If dw_5.Object.repd_tipood[ll_PalletAnguo] = 2 Then
						ls_Registro	=	"E"
						ls_Registro	+=	BuscaInspeccion(dw_5.Object.paen_nroori[ll_PalletAnguo])
						ls_Registro	+=	uo_SelCliente.ClienteRotulado
						ls_Registro	+=	String(dw_5.Object.paen_nroori[ll_PalletAnguo], '0000000')
												
						ll_FilaDet	=	dw_1.InsertRow(0)
						dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
												
						ll_Pallet = dw_5.Object.paen_nroori[ll_PalletAnguo]
						//	Actualiza Inspección del Pallet Nuevo
						ll_fila3 = ids_palletencab.Retrieve(uo_SelCliente.Codigo,uo_SelPlantas.Codigo,ll_Pallet)
						If ll_fila3 > 0 Then
							FOR ll_filas3 = 1 TO ll_Fila3
								ids_palletencab.Object.paen_inspec[ll_filas3] = 1
								ids_palletencab.Object.dest_codigo[ll_filas3] = Integer(ls_Destino)
							NEXT	
						
							If ids_palletencab.Update() = 1 Then 
								Commit;
								If sqlca.SQLCode <> 0 Then
									F_ErrorBaseDatos(sqlca, This.Title)
								Else
									ids_palletencab.ResetUpdate()
								End If
							Else
								F_ErrorBaseDatos(sqlca, This.Title)
								RollBack;										
							End If	
						End If
					Else
						If palletanulado(dw_2.Object.paen_nroori[ll_PalletAnguo]) Then
							ls_Registro	=	"M"
							ls_Registro	+=	BuscaInspeccion(dw_2.Object.paen_nroori[ll_PalletAnguo])
							ls_Registro	+=	uo_SelCliente.ClienteRotulado
							ls_Registro	+=	String(dw_2.Object.paen_nroori[ll_PalletAnguo], '0000000')
							ls_Registro	+=	String(dw_2.Object.repd_totcao[ll_PalletAnguo], '0000')
							ls_Registro	+=	CodigoSagEspecie(dw_2.Object.palletencab_espe_codigo[ll_PalletAnguo])
							ls_Destino  =  String(dw_2.Object.dest_codigo[ll_PalletAnguo], '000')
							ls_Registro	+=	ls_Destino
							ls_Registro	+=	FechaInspeccion(dw_2.Object.paen_numero[ll_PalletAnguo])
							//ls_Registro	+=	'__'
							
							ll_FilaDet	=	dw_1.InsertRow(0)
							dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
													
							ll_Pallet = dw_2.Object.paen_nroori[ll_PalletAnguo]
							//	Actualiza Inspección del Pallet Nuevo
							ll_fila3 = ids_palletencab.Retrieve(uo_SelCliente.Codigo,uo_SelPlantas.Codigo,ll_Pallet)
							If ll_fila3 > 0 Then
								FOR ll_filas3 = 1 TO ll_Fila3
									ids_palletencab.Object.paen_inspec[ll_filas3] = 1
									ids_palletencab.Object.dest_codigo[ll_filas3] = Integer(ls_Destino)
								NEXT
							
								If ids_palletencab.Update() = 1 Then 
									Commit;
									If sqlca.SQLCode <> 0 Then
										F_ErrorBaseDatos(sqlca, This.Title)
									Else
										ids_palletencab.ResetUpdate()
									End If
								Else
									F_ErrorBaseDatos(sqlca, This.Title)
									RollBack;										
								End If	
							End If
						End If
					End If
				NEXT
			/*cambios de altura*/
			ElseIf ii_tiporepa = 1 OR ii_tiporepa = 2 Then
				ll_numero2 = Long(em_numero.Text)
				
				If cbx_pda.Checked and ii_tiporepa = 1 Then
					dw_3.DataObject = 'dw_archivosag_pda'
					dw_3.SetTransObject(SqlCa)
					
					dw_4.DataObject = 'dw_archivosag_pda_agrega'
					dw_4.SetTransObject(SqlCa)
					
					dw_6.DataObject = 'dw_elimina_repa_pda'
					dw_6.SetTransObject(SqlCa)
					
					ll_Filas_elim		= dw_6.Retrieve(uo_SelCliente.Codigo,uo_SelPlantas.Codigo, ll_numero2,Integer(1),1)
				Else												
					ll_Filas_elim		= dw_6.Retrieve(uo_SelPlantas.Codigo, ll_numero2,uo_SelCliente.Codigo)
				End If
				
				ll_Filas			= dw_3.Retrieve(uo_SelPlantas.Codigo, ll_numero2,uo_SelCliente.Codigo,2)
				ll_Filasdw_2		= dw_2.Retrieve(uo_SelPlantas.Codigo, Long(istr_mant.argumento[3]), 0)	
				ll_Filasdw_4		= dw_4.Retrieve(uo_SelPlantas.Codigo, Long(istr_mant.argumento[3]), 0, ii_tiporepa)	
															
				If ll_Filas = -1 Then
					F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Pallets")
				ElseIf ll_Filas = 0 AND NOT cbx_pda.Checked Then
					MessageBox("Atención", "No hay información con Cambio Altura indicado.~r~r" + &
								  "Ingrese otra Operación.", Exclamation!, Ok!)
					pb_grabar.Enabled	= False
				Else
					FOR ll_PalletAnguo_alt = 1 TO dw_6.RowCount()	
						ls_Registro	=	"E"
						ls_Registro	+=	BuscaInspeccion(dw_6.Object.paen_numero[ll_PalletAnguo_alt])
						ls_Registro	+=	uo_SelCliente.ClienteRotulado
						ls_Registro	+=	String(dw_6.Object.paen_numero[ll_PalletAnguo_alt], '0000000')
						
						ll_FilaDet	=	dw_1.InsertRow(0)
						dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
						
					NEXT	
					FOR ll_Fila = 1 TO ll_Filas
						ll_Numero	=	dw_3.Object.paen_numero[ll_Fila]
						If palletanulado(dw_3.Object.paen_numero[ll_Fila]) Then
							ls_Registro	=	"M"
												
							ls_Registro	+=	BuscaInspeccion(dw_3.Object.paen_numero[ll_Fila])
							ls_Destino  =  String(dw_3.Object.dest_codigo[ll_Fila], '000')
							ls_Registro	+=	uo_SelCliente.ClienteRotulado
							ls_Registro	+=	String(dw_3.Object.paen_numero[ll_Fila], '0000000')
							ls_Registro	+=	String(dw_3.Object.paen_ccajas[ll_Fila], '0000')
							ls_Registro	+=	CodigoSagEspecie(dw_3.Object.espe_codigo[ll_Fila])
							ls_Registro	+=	ls_Destino
							ls_Registro	+=	FechaInspeccion(dw_3.Object.paen_numero[ll_Fila])
							//ls_Registro	+=	'__'
							
							ll_FilaDet	=	dw_1.InsertRow(0)
							dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
						End If	
					NEXT
					
					If cbx_pda.Checked Then
						FOR ll_Fila = 1 TO ll_Filasdw_4
							If dw_4.Object.repd_tipood[ll_Fila] = 1  Then
								If palletanulado(dw_4.Object.paen_numero[ll_Fila]) Then
									ls_Registro	=	"A"
								
									ls_Registro	+=	BuscaInspeccion(dw_4.Object.paen_numero[ll_Fila])
									ls_Registro	+=	uo_SelCliente.ClienteRotulado
									ls_Registro	+=	String(dw_4.Object.paen_numero[ll_Fila], '0000000')
									ls_Registro	+=	String(dw_4.Object.pafr_ccajas[ll_Fila], '0000')
									ls_Registro	+=	CodigoSagEspecie(dw_4.Object.espe_codigo[ll_Fila])
									ls_Destino  =  String(dw_4.Object.dest_codigo[ll_Fila], '000')
									ls_Registro	+=	ls_Destino
									ls_Registro	+=	FechaInspeccion(dw_4.Object.paen_numero[ll_Fila])
									//ls_Registro	+=	'__'
									ll_FilaDet	=	dw_1.InsertRow(0)
									dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
								End If		
							End If
						NEXT
					Else	
						FOR ll_Fila = 1 TO ll_Filasdw_4
							If dw_4.Object.repd_tipood[ll_Fila] = 2  Then
								If palletanulado(dw_4.Object.paen_numero[ll_Fila]) Then
									ls_Registro	=	"A"
																
									ls_Registro	+=	BuscaInspeccion(dw_4.Object.paen_numero[ll_Fila])
									ls_Registro	+=	uo_SelCliente.ClienteRotulado
									ls_Registro	+=	String(dw_4.Object.paen_numero[ll_Fila], '0000000')
									ls_Registro	+=	String(dw_4.Object.pafr_ccajas[ll_Fila], '0000')
									ls_Registro	+=	CodigoSagEspecie(dw_4.Object.espe_codigo[ll_Fila])
									ls_Destino  =  String(dw_4.Object.dest_codigo[ll_Fila], '000')
									ls_Registro	+=	ls_Destino
									ls_Registro	+=	FechaInspeccion(dw_4.Object.paen_numero[ll_Fila])
									//ls_Registro	+=	'__'
									ll_FilaDet	=	dw_1.InsertRow(0)
									dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
								End If		
							End If
						NEXT
					End If
				End If
			End If	
		End If
	End If
End If
//	Fin de Archivo

ls_Registro	=	String(ii_PlantaSag, '0000') 
ls_Registro	+=	String((dw_1.Rowcount() - 1), '0000')
dw_1.Object.registro[1] = ls_Registro

ls_Registro	=	"&&" 
ll_FilaDet	=	dw_1.InsertRow(0)

dw_1.Object.registro[ll_FilaDet]	=	ls_Registro

If dw_1.SaveAs(gs_disco+":\GeneradosSAG\" + ls_Archivo, Text!, False) = -1 Then
	MessageBox("Atención","No se pudo generar el archivo " + ls_Archivo)
Else
	UPDATE dbo.repalletenca 
		SET repe_gensag = 1
	WHERE repe_nrosag = :ls_NumeroSAG
	AND 	clie_codigo = :uo_SelCliente.Codigo
	AND	plde_codigo = :uo_SelPlantas.Codigo;
	
	COMMIT;
	
	sle_mensa.text	= "Archivo " + gs_disco+":\GeneradosSAG\ "+ ls_Archivo + " Generado."
End If
end event

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

public function string codigosagespecie (integer ai_especie);String	ls_CodigoSag

SELECT	espe_codsag
	INTO	:ls_CodigoSag
	FROM	dbo.especies
	WHERE	espe_codigo	=	:ai_Especie ;

ls_CodigoSag	=	Trim(ls_CodigoSag) + Fill(" ", 8 - Len(Trim(ls_CodigoSag)))

RETURN ls_CodigoSag
end function

public function integer codigoplantasag (integer planta);Integer	li_codigo

SELECT	plde_codpla
	INTO	:li_Codigo
	FROM	dbo.plantadesp
	WHERE	plde_codigo	=	:uo_SelPlantas.Codigo;

RETURN li_Codigo

end function

public function string codigodestino (long al_nropallet);String		ls_Codigo
Integer	li_tipo, li_codigo
Date		ld_fechai
Long		ll_numero

SELECT max(inpd_fechai)
	INTO :ld_fechai
	FROM dbo.inspecpaldet
	WHERE clie_codigo = :uo_SelCliente.Codigo
	AND   paen_numero = :al_NroPallet ;

SELECT inpe_tipoin, inpe_numero
	INTO :li_tipo, :ll_numero
	FROM dbo.inspecpaldet
	WHERE clie_codigo = :uo_SelCliente.Codigo
	AND   paen_numero = :al_NroPallet
	AND   inpd_fechai = :ld_fechai ;
	
SELECT	dest_codigo
	INTO	:li_Codigo
	FROM	dbo.inspecpalenc
	WHERE	clie_codigo	=	:uo_SelCliente.Codigo
	AND	inpe_tipoin	=	:li_tipo
	AND   inpe_numero =  :ll_numero ;

ls_Codigo = String (li_codigo, '000')

RETURN ls_Codigo
end function

public function string fechainspeccion (long al_nropallet);String		ls_Codigo
Date		ld_fechai

SELECT	Min(inpd_fechai)
	INTO	:ld_fechai
	FROM	dbo.inspecpaldet
	WHERE	clie_codigo	=	:uo_SelCliente.Codigo
	AND   plde_codigo =  :uo_SelPlantas.Codigo
	AND	paen_numero	=	:al_NroPallet ;

ls_Codigo	=	String(ld_fechai, 'yyyymmdd')

RETURN ls_Codigo
end function

public function string buscainspeccion (long al_nropallet);String		ls_Codigo
Integer	li_tipo, li_codigo
Date		ld_fechai
Long		ll_numero

SELECT max(inpd_fechai)
	INTO :ld_fechai
	FROM dbo.inspecpaldet
	WHERE clie_codigo = :uo_SelCliente.Codigo
	AND   paen_numero = :al_NroPallet;

SELECT inpe_tipoin, inpe_numero
	INTO :li_tipo, :ll_numero
	FROM dbo.inspecpaldet
	WHERE clie_codigo = :uo_SelCliente.Codigo
	AND   paen_numero = :al_NroPallet
	AND   inpd_fechai = :ld_fechai ;

ls_Codigo = String (ll_numero, '00000')

RETURN ls_Codigo
end function

public function boolean existemovimiento (long ai_numero);Long		ll_numero

SELECT	count(*)
INTO	:ll_numero
FROM	dbo.repalletenca
WHERE	clie_codigo	= :uo_SelCliente.Codigo
AND	plde_codigo = :uo_SelPlantas.Codigo
AND	repe_nrosag = :ai_numero;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Repalletizados")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Sag no ha sido Definido.~r~rIngrese o seleccione otro Código.", Exclamation!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean palletanulado (long al_nropallet);String	ls_Codigo
Integer	li_tipo, li_codigo
Long		ll_numero
Boolean	lb_retorno
Date		ld_fechai

SELECT paen_tipopa
	INTO :li_tipo
	FROM dbo.palletencab
	WHERE clie_codigo = :uo_SelCliente.Codigo
	AND   paen_numero = :al_NroPallet;

If li_tipo = 2 Then
	SELECT max(inpd_fechai)
		INTO :ld_fechai
		FROM dbo.inspecpaldet
		WHERE clie_codigo = :uo_SelCliente.Codigo
		AND   paen_numero = :al_NroPallet;
	
	SELECT isnull(inpd_nroanu,0)
		INTO :ll_numero
		FROM dbo.inspecpaldet
		WHERE clie_codigo = :uo_SelCliente.Codigo
		AND   paen_numero = :al_NroPallet
		AND   inpd_fechai = :ld_fechai ;
Else	
	ll_numero = 0 
End If

If ll_numero > 0 Then
	lb_retorno = False
Else
	lb_retorno = True
End If	

Return lb_retorno
end function

on w_gene_archivo_repalletizacion_repa_sag.create
this.uo_selcliente=create uo_selcliente
this.uo_selplantas=create uo_selplantas
this.cbx_pda=create cbx_pda
this.dw_6=create dw_6
this.dw_5=create dw_5
this.dw_4=create dw_4
this.dw_3=create dw_3
this.em_numero=create em_numero
this.st_numero=create st_numero
this.st_4=create st_4
this.st_3=create st_3
this.em_fecha=create em_fecha
this.st_5=create st_5
this.st_1=create st_1
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.dw_2=create dw_2
this.dw_1=create dw_1
this.st_8=create st_8
this.sle_mensa=create sle_mensa
this.st_6=create st_6
this.Control[]={this.uo_selcliente,&
this.uo_selplantas,&
this.cbx_pda,&
this.dw_6,&
this.dw_5,&
this.dw_4,&
this.dw_3,&
this.em_numero,&
this.st_numero,&
this.st_4,&
this.st_3,&
this.em_fecha,&
this.st_5,&
this.st_1,&
this.pb_salir,&
this.pb_grabar,&
this.dw_2,&
this.dw_1,&
this.st_8,&
this.sle_mensa,&
this.st_6}
end on

on w_gene_archivo_repalletizacion_repa_sag.destroy
destroy(this.uo_selcliente)
destroy(this.uo_selplantas)
destroy(this.cbx_pda)
destroy(this.dw_6)
destroy(this.dw_5)
destroy(this.dw_4)
destroy(this.dw_3)
destroy(this.em_numero)
destroy(this.st_numero)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.em_fecha)
destroy(this.st_5)
destroy(this.st_1)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.dw_2)
destroy(this.dw_1)
destroy(this.st_8)
destroy(this.sle_mensa)
destroy(this.st_6)
end on

event open;Boolean lb_Cerrar 

x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

If IsNull(uo_SelPlantas.Codigo)Then lb_Cerrar = True
If IsNull(uo_SelCliente.Codigo)Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else	
	DataWindowChild	ldwc_especie, ldwc_embalaje
	
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelPlantas.Inicia(gi_CodPlanta)
	uo_SelCliente.Inicia(gi_CodExport)
								
	ids_palletencab 				= Create DataStore 
	ids_inspeccion					= Create DataStore
	
	ids_palletencab.DataObject = "dw_mues_palletencab_repa"
	ids_inspeccion.DataObject  = "dw_mues_inspecpaldet_repa"
	
	dw_4.SetTransObject(sqlca)
	dw_5.SetTransObject(sqlca)
	dw_6.SetTransObject(sqlca)
	GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
End If
end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_repalletizacion_repa_sag
event destroy ( )
integer x = 759
integer y = 308
integer height = 100
integer taborder = 40
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplantas from uo_seleccion_plantas within w_gene_archivo_repalletizacion_repa_sag
event destroy ( )
integer x = 759
integer y = 424
integer height = 100
integer taborder = 30
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

type cbx_pda from checkbox within w_gene_archivo_repalletizacion_repa_sag
integer x = 763
integer y = 748
integer width = 722
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Repalletizaje por PDA"
end type

type dw_6 from datawindow within w_gene_archivo_repalletizacion_repa_sag
boolean visible = false
integer x = 2080
integer y = 8
integer width = 169
integer height = 136
integer taborder = 80
string title = "none"
string dataobject = "dw_archivorepaanulado"
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within w_gene_archivo_repalletizacion_repa_sag
boolean visible = false
integer x = 2304
integer y = 380
integer width = 160
integer height = 152
integer taborder = 80
string title = "none"
string dataobject = "dw_anulacompletarpal"
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_gene_archivo_repalletizacion_repa_sag
boolean visible = false
integer x = 2080
integer y = 392
integer width = 169
integer height = 136
integer taborder = 70
string title = "none"
string dataobject = "dw_agrupa_repa_sag"
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within w_gene_archivo_repalletizacion_repa_sag
boolean visible = false
integer x = 2304
integer y = 24
integer width = 160
integer height = 152
integer taborder = 70
string title = "none"
string dataobject = "dw_repas_modificados"
borderstyle borderstyle = stylelowered!
end type

type em_numero from editmask within w_gene_archivo_repalletizacion_repa_sag
integer x = 768
integer y = 628
integer width = 343
integer height = 92
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Long  ll_numero
Date	ld_fecha

If This.Text <> "" Then
	If ExisteMovimiento(Long(This.Text)) Then
		This.Text	=	""
		This.SetFocus()
	Else
		istr_mant.argumento[3]	=	String(Long(This.Text), '00000000')
		ll_numero 	= Long(istr_mant.argumento[3])
		
		SELECT Max(repe_tipopa)
		INTO	:ii_tiporepa
		FROM dbo.repalletenca
		WHERE clie_codigo = :uo_SelCliente.Codigo
		AND	plde_codigo = :uo_SelPlantas.Codigo
		AND	repe_nrosag = :ll_numero;
		
		SELECT Max(repe_fecrep)
		INTO	:ld_fecha
		FROM dbo.repalletenca
		WHERE clie_codigo = :uo_SelCliente.Codigo
		AND	plde_codigo = :uo_SelPlantas.Codigo
		AND	repe_nrosag = :ll_numero;
		
		pb_grabar.Enabled = True
		em_fecha.Text = String(ld_fecha)	
	End If
End If
end event

type st_numero from statictext within w_gene_archivo_repalletizacion_repa_sag
integer x = 187
integer y = 640
integer width = 480
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Número S.A.G."
boolean focusrectangle = false
end type

type st_4 from statictext within w_gene_archivo_repalletizacion_repa_sag
integer x = 187
integer y = 440
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_gene_archivo_repalletizacion_repa_sag
integer x = 187
integer y = 324
integer width = 311
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_gene_archivo_repalletizacion_repa_sag
integer x = 1481
integer y = 628
integer width = 443
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
alignment alignment = center!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
string displaydata = "@"
end type

type st_5 from statictext within w_gene_archivo_repalletizacion_repa_sag
integer x = 78
integer y = 68
integer width = 1970
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
string text = "Genera Archivo Plano S.A.G. por Repalletizado"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_1 from statictext within w_gene_archivo_repalletizacion_repa_sag
integer x = 91
integer y = 248
integer width = 1970
integer height = 336
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_gene_archivo_repalletizacion_repa_sag
integer x = 2176
integer y = 856
integer width = 302
integer height = 244
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_gene_archivo_repalletizacion_repa_sag
integer x = 2176
integer y = 564
integer width = 302
integer height = 244
integer taborder = 50
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;
MessageBox("Atención", "Se generará un Archivo de Repalletización.")

Parent.TriggerEvent("ue_guardar")
end event

type dw_2 from datawindow within w_gene_archivo_repalletizacion_repa_sag
boolean visible = false
integer x = 2304
integer y = 192
integer width = 160
integer height = 152
string dataobject = "dw_mues_repalletdeta_gen_nuevo_sag"
borderstyle borderstyle = stylelowered!
end type

event clicked;This.Print()
end event

type dw_1 from datawindow within w_gene_archivo_repalletizacion_repa_sag
boolean visible = false
integer x = 2080
integer y = 192
integer width = 160
integer height = 152
string dataobject = "dw_gene_archivo_saam_plano"
borderstyle borderstyle = stylelowered!
end type

type st_8 from statictext within w_gene_archivo_repalletizacion_repa_sag
integer x = 91
integer y = 584
integer width = 1970
integer height = 288
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type sle_mensa from singlelineedit within w_gene_archivo_repalletizacion_repa_sag
integer x = 123
integer y = 924
integer width = 1902
integer height = 116
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
boolean italic = true
long textcolor = 65535
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_6 from statictext within w_gene_archivo_repalletizacion_repa_sag
integer x = 91
integer y = 868
integer width = 1970
integer height = 224
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

