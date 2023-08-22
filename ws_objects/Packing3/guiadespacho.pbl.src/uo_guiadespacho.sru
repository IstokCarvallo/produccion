$PBExportHeader$uo_guiadespacho.sru
forward
global type uo_guiadespacho from nonvisualobject
end type
end forward

global type uo_guiadespacho from nonvisualobject
end type
global uo_guiadespacho uo_guiadespacho

type variables
Transaction 			SQLProd
Private DataStore			ids_Guia, ids_Source, ids_Termografo, ids_IFCO, ids_Cajas, ids_Pallet

Private 	Long		il_NroGuia = -1
Private 	String		PuertoDestino, NumerosSellos, CantidadSellos, UbicacionSello, GlosaGD, RutRecibidor, Recibidor

Constant	Integer 	_TipoDocto 			= 28
Constant	Integer 	_TipoDoctoDTE 	= 52
Constant Integer 	_Emisor	 			= 1
Constant	String	  	_Separador			= '_'
Constant	String	 	_SeparadorDTE	= '|'
Constant Boolean	_FrutaEmbalada	= True

Private uo_Plantadesp	iuo_Planta
Private uo_Conectividad	iuo_Coneccion
Private n_buscaarchivo	iuo_BuscaArchivo
end variables

forward prototypes
private function boolean of_conectar (integer ai_coneccion)
private function boolean of_desconectar ()
private function boolean of_grabaarchivo (long folio)
private function long of_obtienenroguia (integer tipodocto, integer emisor, transaction at_transaccion)
private subroutine of_insertaregistro (string tag, string referencia)
public function boolean of_generalibroguia ()
public function long of_emiteguia (long planta, long cliente)
public subroutine of_setpuertodestino (string codigo)
public subroutine of_setnumerosellos (string codigo)
public subroutine of_setcantidadsellos (string codigo)
public subroutine of_setglosagd (string codigo)
public subroutine of_setubicacionsellos (string codigo)
private function string of_generatermografos (integer li_cliente, integer li_planta, long li_nroguia)
public subroutine of_setrecibidor (string codigo)
public subroutine of_setrutrecibidor (string codigo)
private function boolean of_generaguia (long planta, long cliente, long movimiento, integer variedad, integer productor, integer calibre, integer consvariedad, integer consembalaje, integer conscalibre, integer resumen)
public function long of_emiteguia (long planta, long cliente, long movimiento, boolean conecta, integer variedad, integer productor, integer calibre, integer consvariedad, integer consembalaje, integer conscalibre, integer resumen)
public function long of_recuperapdf (long al_nroguia, date ad_fecha)
public function boolean of_actualizaestadogd (integer estado, integer cliente, integer planta, long guia, transaction at_transaccion)
end prototypes

private function boolean of_conectar (integer ai_coneccion);SetPointer (HourGlass!)

Boolean	lb_Retorno = True
String		ls_Mensaje

If Not iuo_Coneccion.Existe(ai_Coneccion, SQLca, True) Then
	lb_Retorno = False
Else	
	If iuo_Coneccion.noDBMS = 'ODBC' Then
		SQLProd.ServerName		=	iuo_Coneccion.nomSer
		SQLProd.DataBase			=	iuo_Coneccion.nomBas
		SQLProd.Dbms				= 	iuo_Coneccion.noDBMS
		SQLProd.DbParm			=	"Connectstring='DSN=" + iuo_Coneccion.nomODB + ";UID=" + iuo_Coneccion.Usuario + &
										";PWD=" + iuo_Coneccion.Password + "'// ;PBUseProcOwner = " + '"Yes"'
	ElseIf Mid(iuo_Coneccion.noDBMS,1,3) = 'SNC' or Mid(iuo_Coneccion.noDBMS,1,9) = 'TRACE SNC' Then
		SQLProd.LogId				=	iuo_Coneccion.Usuario
		SQLProd.LogPass			=	iuo_Coneccion.Password
		SQLProd.DbParm			=	"Provider='SQLNCLI10',Database='"+iuo_Coneccion.nomBas+"',TrimSpaces=1"	
		SQLProd.ServerName		=	iuo_Coneccion.nomSer
		SQLProd.DataBase			=	iuo_Coneccion.nomBas
		SQLProd.Dbms				= 	iuo_Coneccion.noDBMS
	End If
	
	CONNECT USING SQLProd;

	If SQLProd.SQLCode = 0 Then
		lb_Retorno	=	True
	Else
		If SQLProd.SQLCode = 18456 Then
			ls_Mensaje = 'User Login Failed'
		ElseIf SQLProd.SQLCode = 18456 Then
			ls_Mensaje = 'No se pudo establecer una coneccion'
		End If
		lb_Retorno	=	False
	End If
End If	
	
SetPointer(Arrow!)

Return	lb_Retorno
end function

private function boolean of_desconectar ();Boolean	lb_Retorno = True

DISCONNECT USING SQLProd;

If SQLProd.SQLCode = 0 Then
	lb_Retorno	=	True
Else
	lb_Retorno	=	False
End If

Return lb_Retorno
end function

private function boolean of_grabaarchivo (long folio);Boolean	lb_Retorno = True
String 	ls_File, ls_Path

SetPointer(HourGlass!)

If DirectoryExists(gs_Ubicacion_DTE) Then
	ls_Path	= gs_Ubicacion_DTE
Else
	If Not DirectoryExists("C:\GeneraGuiaElectronica") Then
		CreateDirectory ("C:\GeneraGuiaElectronica")
		ls_Path	= "C:\GeneraGuiaElectronica\"
	Else
		ls_Path	= "C:\GeneraGuiaElectronica\"
	End If
End If

ls_File	=	ls_Path + String(_TipoDoctoDTE) + _Separador + String(Folio) + _Separador + gstr_ParEmpresa.empr_rutemp + '.txt'

If ids_Guia.SaveAs(ls_File, Text!, False) = -1 Then
	MessageBox('Error', 'No se pùdo generar archivo ('+ ls_File +') con informción solicitda.' , StopSign!, OK! )
	lb_Retorno = False
Else
	MessageBox('Atencion', 'Archivo ('+ ls_File +') generado satisfactoriamente.' , Information!, OK! )
End If

SetPointer(Arrow!)

Return lb_Retorno
end function

private function long of_obtienenroguia (integer tipodocto, integer emisor, transaction at_transaccion);Long	ll_Retorno

DECLARE ObtieneGuia PROCEDURE FOR dba.Cont_GeneraDocumentos	
		  @CentroEmisor 	= :Emisor,
		  @TipoDoc 			= :TipoDocto
	Using at_Transaccion;
		  
EXECUTE ObtieneGuia;

Fetch ObtieneGuia into :ll_Retorno;

Close ObtieneGuia;
		
If at_Transaccion.SQLCode = -1 Then
	F_ErrorBaseDatos(at_Transaccion, "Lectura Procedimiento Cont_GeneraDocumentos, Obtiene Numero de Guia.")
	ll_Retorno =  -1
End If	

Return ll_Retorno
end function

private subroutine of_insertaregistro (string tag, string referencia);Long	ll_New 
String	ls_Registro

//Agrega Referencia
If IsNull(Referencia) Then
	ls_Registro = '<' + Tag + '>'
ElseIf Tag = '' Then
	ls_Registro = Referencia
Else
	ls_Registro = Tag + _SeparadorDTE + Referencia
End If

ll_New = ids_Guia.InsertRow(0)	
ids_Guia.Object.registro[ll_New] = ls_Registro

end subroutine

public function boolean of_generalibroguia ();Boolean	lb_Retorno = True

If of_Conectar(gi_Conecion_GuiaElectronica) Then
	
	INSERT INTO dba.contguiasdespacho (tpdo_codigo, guia_numero, guia_estado, guia_anumod, guia_tipope, guia_fechag, clpr_rut, guia_nomrec, 
												 guia_valnet, guia_poriva, guia_valiva, guia_valtot, guia_valmod, guia_tdrefe, guia_docref, guia_fecref, 
												 ceem_codigo, guia_usuemi, guia_modori, guia_feccre, guia_usumod, guia_fecmod, bode_codigo) 
        VALUES ( 28, :il_NroGuia, 1, NULL, 6, GetDate(), :RutRecibidor, SubString(:Recibidor,1,60), 
		  			0, 0, 0, 0, 0, NULL, NULL, NULL, 
					1, :gstr_us.Nombre, SubString(:gstr_apl.NombreSistema,1,40), GetDate(), NULL, NULL, Null)
		Using SQLProd;
	
	If SQLProd.SQLCode = -1 Then
		F_ErrorBaseDatos(SQLProd, "Lectura Carga de Libro de Guia Electronicas")
		lb_Retorno =  False
	Else
		If of_DesConectar() Then 
		End If
	End If
Else
	MessageBox('Error', 'No se pudo conectar con la base emisora de Folios.', StopSign!, OK!)
End If

Return lb_Retorno
end function

public function long of_emiteguia (long planta, long cliente);If of_Conectar(gi_Conecion_GuiaElectronica) Then
	il_NroGuia = of_ObtieneNroGuia(_TipoDocto, _Emisor, SQLProd)
	If il_NroGuia = -1 Then
		MessageBox('Error', 'No se correlativos disponibles, se utilizo el ultimo.', StopSign!, OK!)
	ElseIf il_NroGuia = -2 Then
		MessageBox('Error', 'No se existen rangos vigentes para correlativos.', StopSign!, OK!)
	Else
		If Not of_DesConectar() Then 
			MessageBox('Error', 'No se pudo desconectare de la base emisora de Folios.', StopSign!, OK!)
		End If
	End If
Else
	MessageBox('Error', 'No se pudo conectar con la base emisora de Folios.', StopSign!, OK!)
End If

Return il_NroGuia
end function

public subroutine of_setpuertodestino (string codigo);If IsNull(Codigo) Then Codigo = ''
This.PuertoDestino = Trim(Codigo)
end subroutine

public subroutine of_setnumerosellos (string codigo);If IsNull(Codigo) Then Codigo = ''
This.NumerosSellos	= Trim(Codigo)


end subroutine

public subroutine of_setcantidadsellos (string codigo);If IsNull(Codigo) Then Codigo = ''
This.CantidadSellos	= Trim(Codigo)


end subroutine

public subroutine of_setglosagd (string codigo);If IsNull(Codigo) Then Codigo = ''
This.GlosaGD	= Trim(Codigo)
end subroutine

public subroutine of_setubicacionsellos (string codigo);If IsNull(Codigo) Then Codigo = ''
This.UbicacionSello	= Trim(Codigo)
end subroutine

private function string of_generatermografos (integer li_cliente, integer li_planta, long li_nroguia);String ls_Retorno = ''
Long	ll_Fila

If ids_Termografo.Retrieve(li_Cliente, li_Planta, li_NroGuia) = -1 Then 
	ls_Retorno = ''
Else
	For ll_Fila = 1 To ids_Termografo.RowCount()
		If ll_Fila = ids_Termografo.RowCount() Then 
			ls_Retorno += Trim(ids_Termografo.Object.defe_termog[ll_Fila])
		Else
			ls_Retorno += Trim(ids_Termografo.Object.defe_termog[ll_Fila]) + _SeparadorDTE
		End If
	Next
End If

Return ls_Retorno
end function

public subroutine of_setrecibidor (string codigo);If IsNull(Codigo) Then Codigo = ''
This.Recibidor	= Trim(Codigo)


end subroutine

public subroutine of_setrutrecibidor (string codigo);If IsNull(Codigo) Then Codigo = ''
This.RutRecibidor	= Trim(Codigo)


end subroutine

private function boolean of_generaguia (long planta, long cliente, long movimiento, integer variedad, integer productor, integer calibre, integer consvariedad, integer consembalaje, integer conscalibre, integer resumen);Boolean	lb_Retorno = True
Long		ll_New, ll_Fila, ll_Linea
String		ls_Referencia, ls_Rut, ls_PuertoOrigen, ls_Puerto, ls_Pais

SetNull(ls_Referencia)
//Movimiento
ll_Fila = ids_Source.Retrieve(Cliente, Planta, Movimiento , 1, Variedad, Productor, Calibre, ConsVariedad, ConsEmbalaje, ConsCalibre, Resumen)
If  ll_Fila	= -1 Then
	MessageBox('Error', 'Error al cargar informacion para generacion de Guia Electronica.', StopSign!, OK!)
	lb_Retorno = False
ElseIf ll_Fila	= 0 Then
	MessageBox('Alerta', 'No se pudo recuperar informacion de Guia Electronica (Filas Recuperadas = ' + String(ll_Fila)+ ')', Exclamation!, OK!)
	lb_Retorno = False
Else
	//Genera Archivo DTE
	ids_Guia.Reset()
	
	of_SetRutRecibidor(ids_Source.Object.embc_nrorut[1])
	of_SetRecibidor(ids_Source.Object.embc_nombre[1])
	
	ls_Rut = String(Long(Mid(ids_Source.Object.embc_nrorut[1], 1, Len(String(ids_Source.Object.embc_nrorut[1])) - 1))) + '-' + &
								Right(ids_Source.Object.embc_nrorut[1], 1)

	//Genera Encabezado
	of_InsertaRegistro('ENCABEZADO', ls_Referencia)
	
	of_InsertaRegistro('Tipo DTE', String(_TipoDoctoDTE))
	of_InsertaRegistro('Folio', String(il_NroGuia))
	of_InsertaRegistro('Fecha de Emision', String(ids_Source.Object.defe_fecdes[1], 'dd-mm-yyyy'))
	of_InsertaRegistro('Ind. traslado de bienes', String(ids_Source.Object.defe_tipotr[1]))
	of_InsertaRegistro('Codigo Traslado', '1')
	of_InsertaRegistro('Tipo Despacho', String(ids_Source.Object.defe_tipode[1]))
	of_InsertaRegistro('Forma Pago', '')
	of_InsertaRegistro('Codigo Vendedor', '')
	
	of_InsertaRegistro('Rut Receptor', ls_Rut)
	of_InsertaRegistro('Codigo Interno Receptor', '')
	of_InsertaRegistro('Razon Social Receptor', ids_Source.Object.embc_nombre[1])
	of_InsertaRegistro('Giro Receptor', Mid(ids_Source.Object.embc_giroem[1], 1, 40)) 
	of_InsertaRegistro('Contacto Receptor', '')
	of_InsertaRegistro('Direccion Receptor', ids_Source.Object.embc_dirori[1])
	of_InsertaRegistro('Comuna Receptor', ids_Source.Object.embc_comuna[1])
	of_InsertaRegistro('Ciudad Receptor', ids_Source.Object.embc_ciudad[1])
	
	ls_Rut = String(Long(Mid(ids_Source.Object.defe_chfrut[1], 1, Len(String(ids_Source.Object.defe_chfrut[1])) - 1))) + '-' + &
								Right(ids_Source.Object.defe_chfrut[1], 1)
								
	of_InsertaRegistro('RUT Chofer', ls_Rut)
	of_InsertaRegistro('xCelular Chofer', ids_Source.Object.defe_celcho[1])
	of_InsertaRegistro('Nombre Chofer', ids_Source.Object.defe_chofer[1])
	of_InsertaRegistro("Patente Transporte", ids_Source.Object.defe_patent[1])
	of_InsertaRegistro("xPatenteCarro", ids_Source.Object.defe_pataco[1])
	
	of_InsertaRegistro('Direccion Origen', gstr_parempresa.empr_direcc) 
	of_InsertaRegistro('Comuna Origen', gstr_parempresa.empr_comuna)
	of_InsertaRegistro('Ciudad Origen', gstr_parempresa.empr_ciudad)

	of_InsertaRegistro('Direccion Destino', ids_Source.Object.embc_direcc[1])
	of_InsertaRegistro('Comuna Destino', ids_Source.Object.embc_comdes[1])
//	of_InsertaRegistro('xPlanta Destino', ids_Source.Object.embc_nomdes[1])
	
	of_InsertaRegistro('Monto Neto', '0')
	of_InsertaRegistro('Monto Exento', '')
	of_InsertaRegistro('Tasa Iva', '19')
	of_InsertaRegistro('Iva', '')
	of_InsertaRegistro('Monto Total', '0')
	of_InsertaRegistro('xMontoEscrito', '')

	If IsNull(ids_Source.Object.defe_glosas[1]) Then
		ls_Referencia = ''
	Else
		ls_Referencia = ids_Source.Object.defe_glosas[1]
	End If
	
	of_InsertaRegistro('xObservaciones', ls_Referencia )//+ " - Traslado de fruta a puerto para ser exportada, No constituye venta")
	
	//Glosa 1
	ls_Referencia = 'Total VGM: ' + String(ids_Source.Object.defe_totvgm[1], '#,##0.00') + &
						', Sello(s) N : ' + CantidadSellos + ' - ' + NumerosSellos + &
						', Ubicación : ' + UbicacionSello +  ', Multiuso : ' + Trim(ids_Source.Object.defe_term01[1]) + '/' + &
						Trim(ids_Source.Object.defe_term02[1]) + '/' + Trim(ids_Source.Object.defe_term03[1])			
	of_InsertaRegistro('xObservaciones1', ls_Referencia)
	//Glosa 2
	ls_Referencia = 'Reserva: ' + Trim(ids_Source.Object.embq_bookin[1]) + &
						', Planilla SAG: ' + String(ids_Source.Object.defe_nrosps[1]) + ', Termografo : '  + &
						of_GeneraTermografos(Cliente, Planta, Movimiento)
	of_InsertaRegistro('xObservaciones2', ls_Referencia)
	
	of_InsertaRegistro('Sello', Mid(NumerosSellos, 1, 20))
	of_InsertaRegistro('Booking', Trim(ids_Source.Object.embq_bookin[1]))
	of_InsertaRegistro('Nombre Transporte', ids_Source.Object.embq_nomnav[1])
	of_InsertaRegistro('xEmbarque', ids_Source.Object.embq_codigo[1])
	
	If ids_Source.Object.embq_ptoori[1] = 0 Or IsNull(ids_Source.Object.embq_ptoori[1]) Then
		ls_PuertoOrigen	= ''
	Else
		ls_PuertoOrigen	=	String(ids_Source.Object.embq_ptoori[1])
	End If
	
	If ids_Source.Object.puer_codadu[1] = 0 Or IsNull(ids_Source.Object.puer_codadu[1]) Then
		ls_Puerto	= ''
	Else
		ls_Puerto	=	String(ids_Source.Object.puer_codadu[1])
	End If
	
	If ids_Source.Object.dest_codigo[1] = 0 Or IsNull(ids_Source.Object.dest_codigo[1]) Then
		ls_Pais	= ''
	Else
		ls_Pais	=	String(ids_Source.Object.dest_codigo[1])
	End If
	
	of_InsertaRegistro('Cod. Puerto Embarque', ls_PuertoOrigen)
	of_InsertaRegistro('Id. Adicional Puerto Embarque', ids_Source.Object.orindespacho[1])
	of_InsertaRegistro('Cod. Puerto Desembarque', ls_Puerto)
	of_InsertaRegistro('Id Adicional Puerto Desembarque', PuertoDestino)
	of_InsertaRegistro('Cod. Pais Destino', ls_Pais)

	of_InsertaRegistro('xcontenedor', ids_Source.Object.defe_nrcont[1])
	of_InsertaRegistro('xConsignatario', ids_Source.Object.reci_nombre[1])
	of_InsertaRegistro('xDUS', ids_Source.Object.embq_nrodus[1])
	of_InsertaRegistro('xASP', ids_Source.Object.defe_nrspsd[1])
	
	of_InsertaRegistro('xUsuario Emision', gstr_us.Nombre)
	of_InsertaRegistro('xLugar Emision', iuo_Planta.Nombre)
	
	//Genera Detalle
	SetNull(ls_Referencia)
	of_InsertaRegistro('DETALLE', ls_Referencia)	
	of_InsertaRegistro('', "Nro.Linea|Tipo codigo|Codigo del Item|Indicador Exencion|Nombre del Item|Descripcion Adicional Item|Cantidad|Unidad de Medida|Precio Unitario Item|Monto Item")
	For ll_Fila = 1 To ids_Source.RowCount()
		ls_Referencia = String(ll_Fila) + _SeparadorDTE
		ls_Referencia += '' + _SeparadorDTE
		ls_Referencia += '' + _SeparadorDTE
		ls_Referencia += '' + _SeparadorDTE
		ls_Referencia += Mid('Cajas de ' + Trim(ids_Source.Object.espe_nombre[ll_Fila]) + ' Exportación ' + Trim(ids_Source.Object.emba_codigo[ll_Fila]) + ' ' + Trim(ids_Source.Object.vari_nombre[ll_Fila]) + ' Cal.' +Trim(ids_Source.Object.pafr_calibr[ll_Fila])  + ' - ' + String(ids_Source.Object.enva_pesone[ll_Fila], '#,##0.00') + ' Kn. ' + String(ids_Source.Object.enva_pesobr[ll_Fila], '#,##0.00') + ' Kb. ' ,1,80)+ _SeparadorDTE
		ls_Referencia += 'Cajas de ' + Trim(ids_Source.Object.espe_nombre[ll_Fila]) + ' Exportación ' + Trim(ids_Source.Object.emba_codigo[ll_Fila]) + ' ' + Trim(ids_Source.Object.vari_nombre[ll_Fila]) + ' Cal.' +Trim(ids_Source.Object.pafr_calibr[ll_Fila])  + ' - ' + String(ids_Source.Object.enva_pesone[ll_Fila], '#,##0.00') + ' Kn. ' + String(ids_Source.Object.enva_pesobr[ll_Fila], '#,##0.00') + ' Kb. ' + _SeparadorDTE
		ls_Referencia += String(ids_Source.Object.paen_ccajas[ll_Fila], "#,##0") + _SeparadorDTE
		ls_Referencia += 'UNID' + _SeparadorDTE
		ls_Referencia += '' + _SeparadorDTE
		ls_Referencia += '0'
		
		of_InsertaRegistro('', ls_Referencia)
	Next
	
	//Genera Detalle IFCO
	ll_Linea = ll_Fila 
	ids_IFCO.Retrieve(Cliente, Planta, Movimiento)
	For ll_Fila = 1 To ids_IFCO.RowCount()
		ls_Referencia = String(ll_Linea) + _SeparadorDTE
		ls_Referencia += '' + _SeparadorDTE
		ls_Referencia += '' + _SeparadorDTE
		ls_Referencia += '2' + _SeparadorDTE
		ls_Referencia += ids_IFCO.Object.Response[ll_Fila] + _SeparadorDTE
		ls_Referencia += ids_IFCO.Object.Response[ll_Fila] + _SeparadorDTE
		ls_Referencia += ''+ _SeparadorDTE
		ls_Referencia += '' + _SeparadorDTE
		ls_Referencia += '' + _SeparadorDTE
		ls_Referencia += '0'
		
		of_InsertaRegistro('', ls_Referencia)
		
		ll_Linea++
	Next

	//Genera Informacion detalle de envases
	If Resumen = 1 Then
		ids_Pallet.Retrieve(Cliente, Planta, Movimiento, 1, Variedad, Productor, Calibre, ConsVariedad, ConsEmbalaje, ConsCalibre)
		For ll_Fila = 1 To ids_Pallet.RowCount()
			ls_Referencia = String(ll_Linea) + _SeparadorDTE
			ls_Referencia += '' + _SeparadorDTE
			ls_Referencia += '' + _SeparadorDTE
			ls_Referencia += '2' + _SeparadorDTE
			ls_Referencia += String(ids_Pallet.Object.Cont[ll_Fila], '#,##0') + ' --> ' + String(ids_Pallet.Object.copa_anchos[ll_Fila], '#,##0.00') + ' X ' + String(ids_Pallet.Object.copa_largos[ll_Fila], '#,##0.00') + _SeparadorDTE
			ls_Referencia += String(ids_Pallet.Object.Cont[ll_Fila], '#,##0') + ' --> ' + String(ids_Pallet.Object.copa_anchos[ll_Fila], '#,##0.00') + ' X ' + String(ids_Pallet.Object.copa_largos[ll_Fila], '#,##0.00') + _SeparadorDTE
			ls_Referencia += ''+ _SeparadorDTE
			ls_Referencia += '' + _SeparadorDTE
			ls_Referencia += '' + _SeparadorDTE
			ls_Referencia += '0'
			
			of_InsertaRegistro('', ls_Referencia)
			
			ll_Linea++
		Next
		
		ids_Cajas.Retrieve(Cliente, Planta, Movimiento, 1, Variedad, Productor, Calibre, ConsVariedad, ConsEmbalaje, ConsCalibre)
		For ll_Fila = 1 To ids_Cajas.RowCount()
			ls_Referencia = String(ll_Linea) + _SeparadorDTE
			ls_Referencia += '' + _SeparadorDTE
			ls_Referencia += '' + _SeparadorDTE
			ls_Referencia += '2' + _SeparadorDTE
			ls_Referencia += String(ids_Cajas.Object.Cont[ll_Fila], '#,##0') + ' X ' + String(ids_Cajas.Object.cajas[ll_Fila], '#,##0.00') + _SeparadorDTE
			ls_Referencia += String(ids_Cajas.Object.Cont[ll_Fila], '#,##0') + ' X ' + String(ids_Cajas.Object.cajas[ll_Fila], '#,##0.00') + _SeparadorDTE
			ls_Referencia += ''+ _SeparadorDTE
			ls_Referencia += '' + _SeparadorDTE
			ls_Referencia += '' + _SeparadorDTE
			ls_Referencia += '0'
			
			of_InsertaRegistro('', ls_Referencia)
			
			ll_Linea++
		Next	
	End If
	
	SetNull(ls_Referencia)
	//Genera Referencia
	of_InsertaRegistro('REFERENCIA', ls_Referencia)
	of_InsertaRegistro('', "Nro Linea Referencia|Tipo Documento Referencia|Folio Referencia|Fecha Referencia|Codigo Referencia")
	
	//Graba Archivo
	of_GrabaArchivo(il_NroGuia)
End If

Return lb_Retorno 
end function

public function long of_emiteguia (long planta, long cliente, long movimiento, boolean conecta, integer variedad, integer productor, integer calibre, integer consvariedad, integer consembalaje, integer conscalibre, integer resumen);If Conecta Then
	If of_Conectar(gi_Conecion_GuiaElectronica) Then
		il_NroGuia = of_ObtieneNroGuia(_TipoDocto, _Emisor, SQLProd)
		If il_NroGuia = -1 Then
			MessageBox('Error', 'No se correlativos disponibles, se utilizo el ultimo.', StopSign!, OK!)
		ElseIf il_NroGuia = -2 Then
			MessageBox('Error', 'No se existen rangos vigentes para correlativos.', StopSign!, OK!)
		Else
			If of_DesConectar() Then 
				of_GeneraGuia(Planta, Cliente, Movimiento, Variedad, Productor, Calibre, ConsVariedad, ConsEmbalaje, ConsCalibre, Resumen)
			Else
				MessageBox('Error', 'No se pudo desconectare de la base emisora de Folios.', StopSign!, OK!)
			End If
		End If
	Else
		MessageBox('Error', 'No se pudo conectar con la base emisora de Folios.', StopSign!, OK!)
	End If
Else
	il_NroGuia	= Movimiento
	of_GeneraGuia(Cliente, Planta, Movimiento, Variedad, Productor, Calibre, ConsVariedad, ConsEmbalaje, ConsCalibre, Resumen)
End If 

Return il_NroGuia
end function

public function long of_recuperapdf (long al_nroguia, date ad_fecha);Long	ll_Retorno = -1
String	ls_Path, ls_Directorio
Int		li_Contador =	0

If IsNull(al_NroGuia) Then al_NroGuia = il_NroGuia
ls_Directorio = String(ad_Fecha, 'yyyy-mm') + '\'

If DirectoryExists(gs_Ubicacion_PDFDTE + ls_Directorio) Then
	ls_Path = gs_Ubicacion_PDFDTE + ls_Directorio + String(_TipoDoctoDTE) + '_' + String(al_NroGuia) + '*C1.PDF'
	
	Do 
		ll_Retorno = iuo_BuscaArchivo.AbrirDocumento(ls_Path)
		
		If ll_Retorno = -1 Then 
			li_Contador++
			Sleep(5)
		End If
	Loop Until li_Contador = 3 Or ll_Retorno = 1
Else
	MessageBox('Error', 'No se encuentra la carpeta(' + gs_Ubicacion_PDFDTE + ') para acceder a documentos PDF.~n~nFavor Contactarse con Informatica', StopSign!, OK!)
	ll_Retorno = -1
End If

Return ll_Retorno
end function

public function boolean of_actualizaestadogd (integer estado, integer cliente, integer planta, long guia, transaction at_transaccion);Boolean	lb_Retorno = True

Update dbo.despafrigoen
	Set defe_guiaem = :Estado
	Where plde_codigo 	=	:Planta
		And clie_codigo		=	:Cliente
		And defe_guides	=	:Guia
Using	at_Transaccion;

If at_Transaccion.SQLCode = -1 Then
	F_ErrorBaseDatos(at_Transaccion, "Falló Actualizacion Estado Emision Guia Electronica.")
	lb_Retorno =  False
End If

Return lb_Retorno
end function

on uo_guiadespacho.create
call super::create
TriggerEvent( this, "constructor" )
end on

on uo_guiadespacho.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event constructor;SQLProd			=	Create Transaction
ids_Guia			=	Create DataStore
ids_Source		=	Create DataStore
ids_Termografo	=	Create DataStore
ids_IFCO				=	Create DataStore
ids_Pallet			=	Create DataStore
ids_Cajas			=	Create DataStore
iuo_Planta			=	Create uo_Plantadesp
iuo_Coneccion		=	Create uo_Conectividad
iuo_BuscaArchivo	=	Create n_buscaarchivo	

ids_Cajas.DataObject = 'dw_info_guia_despacho_acumulacajas'
ids_Cajas.SetTransObject(Sqlca)

ids_Pallet.DataObject = 'dw_info_guia_despacho_acumulacodpallet'
ids_Pallet.SetTransObject(Sqlca)

ids_IFCO.DataObject = 'dw_info_ifco'
ids_IFCO.SetTransObject(Sqlca)

ids_Termografo.DataObject = 'dw_info_muestra_termografos'
ids_Termografo.SetTransObject(Sqlca)

ids_Source.DataObject = 'dw_info_guia_despacho_cal'
ids_Source.SetTransObject(Sqlca)

ids_Guia.DataObject = 'dw_guiadespacho_dte'
ids_Guia.SetTransObject(Sqlca)

If Not iuo_Planta.Existe(gi_CodPlanta, True, Sqlca) Then
	Messagebox('Error', 'Planta configurada en Parametros no Existe.', StopSign!, OK!)
End If
end event

event destructor;Destroy iuo_Coneccion
Destroy SQLProd
Destroy ids_Guia
Destroy ids_Source
Destroy ids_Termografo
Destroy ids_IFCO
Destroy ids_Pallet
Destroy iuo_Planta
Destroy iuo_BuscaArchivo

end event

