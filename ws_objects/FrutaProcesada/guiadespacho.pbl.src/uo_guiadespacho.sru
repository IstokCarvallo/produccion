$PBExportHeader$uo_guiadespacho.sru
forward
global type uo_guiadespacho from nonvisualobject
end type
end forward

global type uo_guiadespacho from nonvisualobject
end type
global uo_guiadespacho uo_guiadespacho

type variables
Private Transaction	SQLProd
Private DataStore	ids_Guia, ids_Source, ids_Source_Frut, ids_Termografo, ids_IFCO, ids_Cajas, ids_Pallet, ids_CSG, ids_CSP, ids_Source_GDE

Private 	Long		il_NroGuia = -1, IVA
Private	Integer	TipoTransporte
Private 	String		PuertoDestino, NumerosSellos, CantidadSellos, UbicacionSello, GlosaGD, RutRecibidor, Recibidor, &
						RUT, Direccion, Ciudad, Comuna

Private Constant	Integer 	_TipoDocto 			= 28
Private Constant	Integer 	_TipoDoctoDTE 	= 52
Private Constant 	Integer 	_Emisor	 			= 1
Private Constant	String	  	_Separador			= '_'
Private Constant	String	 	_SeparadorDTE	= '|'

Private uo_Plantadesp	iuo_Planta
Private uo_Conectividad	iuo_Coneccion
Private n_buscaarchivo	iuo_BuscaArchivo

end variables

forward prototypes
private function boolean of_conectar (integer ai_coneccion)
private function boolean of_desconectar ()
private function long of_obtienenroguia (integer tipodocto, integer emisor, transaction at_transaccion)
private subroutine of_insertaregistro (string tag, string referencia)
public subroutine of_setpuertodestino (string codigo)
public subroutine of_setnumerosellos (string codigo)
public subroutine of_setcantidadsellos (string codigo)
public subroutine of_setglosagd (string codigo)
public subroutine of_setubicacionsellos (string codigo)
private function string of_generatermografos (integer li_cliente, integer li_planta, long li_nroguia)
public function long of_emiteguia (long planta, long cliente, integer tipo)
public function boolean of_generalibroguia (integer tipo)
public function boolean of_actualizaestadogd (integer estado, integer cliente, integer planta, long guia, integer tipo, transaction at_transaccion)
private function boolean of_grabaarchivo (long folio, integer tipo)
public function long of_recuperapdf (long al_nroguia, date ad_fecha, integer tipo)
private subroutine of_setdireccion (string codigo)
private subroutine of_setcomuna (string codigo)
private subroutine of_setcuidad (string codigo)
private subroutine of_setrut (string codigo)
private subroutine of_setrecibidor (string codigo)
private subroutine of_setrutrecibidor (string codigo)
private function boolean of_datosempresa (integer tipo)
public function integer of_ivaplanta (integer ai_planta)
public function boolean of_anulaguiadespacho (integer estado, integer cliente, integer planta, long guia, integer tipo, transaction at_transaccion)
public function boolean of_generaanulaguia (integer ai_cliente, integer ai_planta, long al_guia, long al_numero, integer ai_tipo, string as_embarque, integer ai_modulo, string as_motivo)
public function long of_emiteguia_fruticola (long planta, long cliente, integer movimiento)
private function boolean of_generaguia_fruticola (long planta, long cliente, long movimiento)
private function string of_codigocsg (integer cliente, integer planta, long planilla)
private function string of_codigocsp (integer cliente, integer planta, long planilla)
private subroutine of_settipotransporte (integer codigo)
public function long of_emiteguia (long planta, long cliente, long movimiento, boolean conecta, long variedad, integer productor, integer calibre, integer consvariedad, integer consembalaje, integer conscalibre, integer resumen, integer tipo, long comprobante)
private function boolean of_generaguia (long planta, long cliente, long movimiento, long variedad, integer productor, integer calibre, integer consvariedad, integer consembalaje, integer conscalibre, integer resumen, integer tipo, long comprobante)
private function boolean of_generaguia_fruticolagde (long planta, long cliente, long movimiento, integer tipo)
public function long of_emiteguia_fruticolagde (long planta, long cliente, integer movimiento, integer tipo)
public function boolean of_generalibroguia_gde (integer tipo)
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
		SQLProd.DbParm			=	"Provider='SQLNCLI11',Database='"+iuo_Coneccion.nomBas+"',TrimSpaces=1"	
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
		Else//If SQLProd.SQLCode = 18456 Then
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

private function long of_obtienenroguia (integer tipodocto, integer emisor, transaction at_transaccion);Long	ll_Retorno

DECLARE ObtieneGuia PROCEDURE FOR Cont_GeneraDocumentos	
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

public function long of_emiteguia (long planta, long cliente, integer tipo);Integer li_Conexion

If Tipo = 1 Then 
	li_Conexion = gstr_ParEmpresa.Conecion_GuiaElectronica
Else
	li_Conexion = gstr_ParEmpresa.Conecion_GuiaElectronica_Comer
End If

If of_Conectar(li_Conexion) Then
	il_NroGuia = of_ObtieneNroGuia(_TipoDocto, _Emisor, SQLProd)
	If il_NroGuia = -1 Then
		MessageBox('Error SQLProd', 'No hay correlativos disponibles, se utilizo el ultimo.', StopSign!, OK!)
	ElseIf il_NroGuia = -2 Then
		MessageBox('Error SQLProd', 'No existen rangos vigentes para correlativos.', StopSign!, OK!)
	Else
		If Not of_DesConectar() Then 
			MessageBox('Error', 'No pudo, desconectare de la base emisora de Folios.', StopSign!, OK!)
		End If
	End If
Else
//	MessageBox('Error', 'No se pudo conectar con la base emisora de Folios.', StopSign!, OK!)
	il_NroGuia = of_ObtieneNroGuia(_TipoDocto, _Emisor, SQLCA)
	If il_NroGuia = -1 Then
		MessageBox('Error SQLCA', 'No hay correlativos disponibles, se utilizo el ultimo.', StopSign!, OK!)
	ElseIf il_NroGuia = -2 Then
		MessageBox('Error SQLCA', 'No existen rangos vigentes para correlativos.', StopSign!, OK!)
	End If
End If

Return il_NroGuia
end function

public function boolean of_generalibroguia (integer tipo);Boolean	lb_Retorno = True
Integer 	li_Conexion
Long		ll_Neto = 0, ll_Iva = 0, ll_Total = 0

If Tipo = 1 Then 
	li_Conexion = gstr_ParEmpresa.Conecion_GuiaElectronica
Else
	li_Conexion = gstr_ParEmpresa.Conecion_GuiaElectronica_Comer
End If

If of_Conectar(li_Conexion) Then
	
	If Tipo = 1 Or Tipo = 0 Then 
		INSERT INTO dba.contguiasdespacho (tpdo_codigo, guia_numero, guia_estado, guia_anumod, guia_tipope, guia_fechag, clpr_rut, guia_nomrec, 
													 guia_valnet, guia_poriva, guia_valiva, guia_valtot, guia_valmod, guia_tdrefe, guia_docref, guia_fecref, 
													 ceem_codigo, guia_usuemi, guia_modori, guia_feccre, guia_usumod, guia_fecmod, bode_codigo) 
			  VALUES ( 28, :il_NroGuia, 1, NULL, :This.TipoTransporte, GetDate(), :This.RutRecibidor, SubString(:This.Recibidor,1,60), 
						0, 0, 0, 0, 0, NULL, NULL, NULL, 
						1, :gstr_us.Nombre, SubString(:gstr_apl.NombreSistema,1,40), GetDate(), NULL, NULL, Null)
			Using SQLProd;
	Else
		ll_Neto	=	ids_Source.Object.Total_Neto[1]
		ll_Iva		=	ids_Source.Object.Total_Iva[1]
		ll_Total	=	ids_Source.Object.Total[1]
		
		INSERT INTO dba.contguiasdespacho (tpdo_codigo, guia_numero, guia_estado, guia_anumod, guia_tipope, guia_fechag, clpr_rut, guia_nomrec, 
											 guia_valnet, guia_poriva, guia_valiva, guia_valtot, guia_valmod, guia_tdrefe, guia_docref, guia_fecref, 
											 ceem_codigo, guia_usuemi, guia_modori, guia_feccre, guia_usumod, guia_fecmod, bode_codigo) 
			VALUES ( 28, :il_NroGuia, 1, NULL, :This.TipoTransporte, GetDate(), :This.RutRecibidor, SubString(:This.Recibidor,1,60), 
						:ll_Neto, :IVA, :ll_Iva, :ll_Total,
						0, NULL, NULL, NULL, 
						1, :gstr_us.Nombre, SubString(:gstr_apl.NombreSistema,1,40), GetDate(), NULL, NULL, Null)
			Using SQLProd;
	End If
	
	If SQLProd.SQLCode = -1 Then
		F_ErrorBaseDatos(SQLProd, "Lectura Carga de Libro de Guia Electronicas")
		lb_Retorno =  False
	Else
		If of_DesConectar() Then 
		End If
	End If
Else
	MessageBox('Error', 'No se pudo conectar con la base para generar libro de Guias.', StopSign!, OK!)
End If

Return lb_Retorno
end function

public function boolean of_actualizaestadogd (integer estado, integer cliente, integer planta, long guia, integer tipo, transaction at_transaccion);Boolean	lb_Retorno = True

If Tipo = 1 Then 
	Update dbo.despafrigoen
		Set defe_guiaem = :Estado
		Where plde_codigo 	=	:Planta
			And clie_codigo		=	:Cliente
			And defe_guides	=	:Guia
	Using	at_Transaccion;
ElseIf Tipo = 2 Then 
	Update dbo.spro_movtoenvaenca
		Set meen_guiemi = :Estado
		Where plde_codigo 	=	:Planta
			And clie_codigo		=	:Cliente
			And meen_guisii	=	:Guia
	Using	at_Transaccion;	
ElseIf Tipo = 3 Then 
	Update dbo.spro_movtofrutacomenca
		Set mfco_guiemi = :Estado
		Where plde_codigo 	=	:Planta
			And clie_codigo		=	:Cliente
			And mfco_guisii	=	:Guia
	Using	at_Transaccion;
ElseIf Tipo = 4 Then 
	Update dbo.spro_movtofrutagranenca
		Set mfge_guiemi = :Estado
		Where plde_codigo 	=	:Planta
			And clie_codigo		=	:Cliente
			And mfge_guisii	=	:Guia
	Using	at_Transaccion;
End If

If at_Transaccion.SQLCode = -1 Then
	F_ErrorBaseDatos(at_Transaccion, "Falló Actualizacion Estado Emision Guia Electronica.")
	lb_Retorno =  False
End If

Return lb_Retorno
end function

private function boolean of_grabaarchivo (long folio, integer tipo);Boolean	lb_Retorno = True
String 	ls_File, ls_Path, ls_Ubicacion

SetPointer(HourGlass!)

If of_DatosEmpresa(Tipo) Then
	If Tipo = 1 Then
		ls_Ubicacion = gstr_ParEmpresa.Ubicacion_DTE
	Else
		ls_Ubicacion = gstr_ParEmpresa.Ubicacion_DTE_Comercial
	End If
	
//	If DirectoryExists(ls_Ubicacion) Then
		ls_Path	= ls_Ubicacion
//	Else
		If Not DirectoryExists("C:\GeneraGuiaElectronica") Then CreateDirectory ("C:\GeneraGuiaElectronica")
//			ls_Path	= "C:\GeneraGuiaElectronica\"
//		Else
//			ls_Path	= "C:\GeneraGuiaElectronica\"
//		End If
//	End If

	ls_File	=	"C:\GeneraGuiaElectronica\" + String(_TipoDoctoDTE) + _Separador + String(Folio) + _Separador + This.Rut + '.txt'
	ids_Guia.SaveAs(ls_File, Text!, False)
	
	ls_File	=	ls_Path + String(_TipoDoctoDTE) + _Separador + String(Folio) + _Separador + This.Rut + '.txt'
	
	If ids_Guia.SaveAs(ls_File, Text!, False) = -1 Then
		MessageBox('Error', 'No se pùdo generar archivo ('+ ls_File +') con información solicitda.' , StopSign!, OK! )
		lb_Retorno = False
	Else
		MessageBox('Atencion', 'Archivo ('+ ls_File +') generado satisfactoriamente.' , Information!, OK! )
	End If
Else
	If Not DirectoryExists("C:\GeneraGuiaElectronica") Then CreateDirectory ("C:\GeneraGuiaElectronica")
	ls_File	=	"C:\GeneraGuiaElectronica\" + String(_TipoDoctoDTE) + _Separador + String(Folio) + _Separador + This.Rut + '.txt'
	ids_Guia.SaveAs(ls_File, Text!, False)
	
//	lb_Retorno = False
End If

SetPointer(Arrow!)

Return lb_Retorno
end function

public function long of_recuperapdf (long al_nroguia, date ad_fecha, integer tipo);Long	ll_Retorno = -1
String	ls_Path, ls_Directorio, ls_Ubicacion
Int		li_Contador =	0

If IsNull(al_NroGuia) Then al_NroGuia = il_NroGuia
ls_Directorio = String(ad_Fecha, 'yyyy-mm') + '\'

If Tipo = 1 Then
	ls_Ubicacion = gstr_ParEmpresa.Ubicacion_PDFDTE
Else
	ls_Ubicacion = gstr_ParEmpresa.Ubicacion_PDFDTE_Comercial
End If

If DirectoryExists(ls_Ubicacion + ls_Directorio) Then
	ls_Path = ls_Ubicacion + ls_Directorio + String(_TipoDoctoDTE) + '_' + String(al_NroGuia) + '*C1.PDF'
	
	Do 
		ll_Retorno = iuo_BuscaArchivo.AbrirDocumento(ls_Path)
		
		If ll_Retorno = -1 Then 
			li_Contador++
			Sleep(5)
		End If
	Loop Until li_Contador = 3 Or ll_Retorno = 1
Else
	MessageBox('Error', 'No se encuentra la carpeta(' + ls_Ubicacion + ls_Directorio + ') para acceder a documentos PDF.~n~nFavor Contactarse con Informatica', StopSign!, OK!)
	ll_Retorno = -1
End If

Return ll_Retorno
end function

private subroutine of_setdireccion (string codigo);If IsNull(Codigo) Then Codigo = ''
This.Direccion	= Trim(Codigo)
end subroutine

private subroutine of_setcomuna (string codigo);If IsNull(Codigo) Then Codigo = ''
This.Comuna	= Trim(Codigo)
end subroutine

private subroutine of_setcuidad (string codigo);If IsNull(Codigo) Then Codigo = ''
This.Ciudad	= Trim(Codigo)
end subroutine

private subroutine of_setrut (string codigo);If IsNull(Codigo) Then Codigo = ''
This.RUT	= Trim(Codigo)
end subroutine

private subroutine of_setrecibidor (string codigo);If IsNull(Codigo) Then Codigo = ''
This.Recibidor	= Trim(Codigo)


end subroutine

private subroutine of_setrutrecibidor (string codigo);If IsNull(Codigo) Then Codigo = ''
This.RutRecibidor	= Trim(Codigo)


end subroutine

private function boolean of_datosempresa (integer tipo);Boolean	lb_Retorno = True
Integer	li_Conexion

If Tipo = 1 Then 
	li_Conexion = gstr_ParEmpresa.Conecion_GuiaElectronica
Else
	li_Conexion = gstr_ParEmpresa.Conecion_GuiaElectronica_Comer
End If

If of_Conectar(li_Conexion) Then
	Select empr_rutemp, empr_comuna, empr_ciudad, empr_direcc
		Into :Rut, :Comuna, :Ciudad, :Direccion
		 From dba.parempresa
	Using	SQLProd;
	
	If SQLProd.SQLCode = -1 Then
		F_ErrorBaseDatos(SQLProd, "No se pudo Cargar Datos de la Empresa Emisora.")
		lb_Retorno =  False
	Else
		of_DesConectar()
	End If
Else
	Select empr_rutemp, empr_comuna, empr_ciudad, empr_direcc
		Into :Rut, :Comuna, :Ciudad, :Direccion
	 	From dbo.parempresa
	Using	SQLCA;
	
	If SQLCA.SQLCode = -1 Then
		F_ErrorBaseDatos(SQLProd, "No se pudo Cargar Datos de la Empresa Emisora.")
		lb_Retorno =  False
	End If
	
//	MessageBox('Error', 'No se pudo conectar con la base Emisora(Datos Empresa).', StopSign!, OK!)
//	lb_Retorno =  False
End If

Return lb_Retorno
end function

public function integer of_ivaplanta (integer ai_planta);Dec{2}	li_Retorno

SELECT  IsNull(prpa_poriva, 0)
	Into :li_Retorno
	FROM dbo.spro_paramplanta
	Where plde_codigo = :ai_Planta
	USING	SQLCA; 
	
If SQLCA.SQLCode = -1 Then
	F_ErrorBaseDatos(SQLCA, "Lectura de Tabla Parametros de Planta")
	li_Retorno	=	-1
ElseIf SQLCA.SQLCode = 100 Then
	li_Retorno	=	-1
Else
	IF li_Retorno = 0 Then
		li_Retorno	=	-1
	Else
		li_Retorno	=	li_Retorno * 100
	End If
End If

Return Integer(li_Retorno)
end function

public function boolean of_anulaguiadespacho (integer estado, integer cliente, integer planta, long guia, integer tipo, transaction at_transaccion);Boolean	lb_Retorno = True

If Tipo = 1 Then 
	Update dbo.despafrigoen
		Set	defe_guiaem = :Estado,
				defe_guides	=	Null
		Where plde_codigo 	=	:Planta
			And clie_codigo		=	:Cliente
			And defe_guides	=	:Guia
	Using	at_Transaccion;
ElseIf Tipo = 2 Then 
	Update dbo.spro_movtoenvaenca
		Set 	meen_guiemi = :Estado,
				meen_guisii	=	Null
		Where plde_codigo 	=	:Planta
			And clie_codigo		=	:Cliente
			And meen_guisii	=	:Guia
	Using	at_Transaccion;	
ElseIf Tipo = 3 Then 
	Update dbo.spro_movtofrutacomenca
		Set 	mfco_guiemi = :Estado,
				mfco_guisii	=	Null
		Where plde_codigo 	=	:Planta
			And clie_codigo		=	:Cliente
			And mfco_guisii	=	:Guia
	Using	at_Transaccion;
ElseIf Tipo = 4 Then 
	Update dbo.spro_movtofrutagranenca
		Set 	mfge_guiemi = :Estado,
				mfge_guisii	=	Null
		Where plde_codigo 	=	:Planta
			And clie_codigo		=	:Cliente
			And mfge_guisii	=	:Guia
	Using	at_Transaccion;
End If

If at_Transaccion.SQLCode = -1 Then
	F_ErrorBaseDatos(at_Transaccion, "Falló Actualizacion Estado Emision Guia Electronica.")
	lb_Retorno =  False
End If

Return lb_Retorno
end function

public function boolean of_generaanulaguia (integer ai_cliente, integer ai_planta, long al_guia, long al_numero, integer ai_tipo, string as_embarque, integer ai_modulo, string as_motivo);Boolean	lb_Retorno = True
String		ls_Cuerpo, ls_Modulo, ls_Asunto,ls_Error
Integer	li_Correo
uo_Mail	iuo_Mail


iuo_Mail	=	Create uo_Mail

Choose Case ai_Modulo
	Case 1
		ls_Modulo = 'Fruta Embalada'
	Case 2
		ls_Modulo = 'Envases'
	Case 3
		ls_Modulo = 'Fruta Comercial'
	Case 4
		ls_Modulo = 'Fruta Granel'
End Choose
	
	
INSERT INTO dbo.spro_anulaguia(clie_codigo, plde_codigo, angu_nrogui, angu_fechaa, angu_numero, tpmv_codigo, 
											embq_codigo,angu_usuari, angu_comput, angu_modulo, angu_motivo) 
	  VALUES ( :ai_Cliente, :ai_Planta, :al_Guia, GetDate(), :al_Numero, :ai_Tipo,
				:as_Embarque, :gstr_us.Nombre, :gstr_us.Computador, :ai_Modulo, :as_Motivo)
	Using SQLCA;

If SQLCA.SQLCode = -1 Then
	F_ErrorBaseDatos(SQLCA, "Error Carga de Tabla Anulacion Guias Despachos")
	lb_Retorno =  False
Else	
	If IsNull(gstr_parempresa.Correo_Anulacion_DTE) Or gstr_parempresa.Correo_Anulacion_DTE = '' Then
		MessageBox('Error', 'No se pudo enviar correo con informe de anulacion, ya que no existe Correo Configurado.', StopSign!, Ok!)
	Else
		ls_Asunto = "Anulacion de Guias Despacho SII"
		ls_Cuerpo  = 'Estimados:~n~n' + 'Se anulo la Guia De Despacho Numero: ' + String(al_Numero, '00000000') + '~n~n' + &
					'Del modulo: ' + ls_Modulo +  '~n~n' + &
					'Por el usuario: ' + gstr_Us.Nombre + ' desde el computador: ' + gstr_Us.Computador + ' con fecha: ' + String(Today(), 'dd/mm/yyyy')
		
		iuo_Mail.Of_Send({gstr_parempresa.Correo_Anulacion_DTE}, ls_Asunto, ls_Cuerpo, 2)
	End If
End If

Destroy iuo_Mail
Return lb_Retorno
end function

public function long of_emiteguia_fruticola (long planta, long cliente, integer movimiento);
If of_Conectar(gstr_ParEmpresa.Conecion_GuiaElectronica) Then
	il_NroGuia = of_ObtieneNroGuia(_TipoDocto, _Emisor, SQLProd)
	If il_NroGuia = -1 Then
		MessageBox('Error', 'No se correlativos disponibles, se utilizo el ultimo.', StopSign!, OK!)
	ElseIf il_NroGuia = -2 Then
		MessageBox('Error', 'No se existen rangos vigentes para correlativos.', StopSign!, OK!)
	Else
		If of_DesConectar() Then 
			of_GeneraGuia_Fruticola(Planta, Cliente, Movimiento)
		Else
			MessageBox('Error', 'No se pudo desconectar de la base de Folios.', StopSign!, OK!)
		End If
	End If
Else
	il_NroGuia = of_ObtieneNroGuia(_TipoDocto, _Emisor, SQLCA)
	If il_NroGuia = -1 Then
		MessageBox('Error', 'No se correlativos disponibles, se utilizo el ultimo.', StopSign!, OK!)
	ElseIf il_NroGuia = -2 Then
		MessageBox('Error', 'No se existen rangos vigentes para correlativos.', StopSign!, OK!)
	Else
		of_GeneraGuia_Fruticola(Planta, Cliente, Movimiento)
	End If
	
//	MessageBox('Error', 'No se pudo conectar con la base emisora de Folios.', StopSign!, OK!)
End If

Return il_NroGuia
end function

private function boolean of_generaguia_fruticola (long planta, long cliente, long movimiento);Boolean	lb_Retorno = True
Long		ll_New, ll_Fila
String		ls_Referencia, ls_Rut 

SetNull(ls_Referencia)
//Movimiento
If ids_Source_Frut.Retrieve(Cliente, Planta, Movimiento , 1, il_NroGuia) = -1 Then
	MessageBox('Error', 'No se pudo recuperar informacion de Guia', StopSign!, OK!)
	lb_Retorno = False
Else
	//Genera Archivo DTE
	ids_Guia.Reset()
	
	of_SetRutRecibidor(ids_Source_Frut.Object.embc_nrorut[1])
	of_SetRecibidor(ids_Source_Frut.Object.embc_nombre[1])
	of_SetTipoTransporte(ids_Source_Frut.Object.defe_tipotr[1])
	
	//Genera Encabezado
	of_InsertaRegistro('ENCABEZADO', ls_Referencia)
	
	of_InsertaRegistro('Tipo DTE', String(_TipoDoctoDTE))
	of_InsertaRegistro('Folio', String(il_NroGuia))
	of_InsertaRegistro('Fecha de Emision', String(ids_Source_Frut.Object.defe_fecdes[1], 'dd-mm-yyyy'))
	of_InsertaRegistro('Ind. traslado de bienes', String(ids_Source_Frut.Object.defe_tipotr[1]))
	of_InsertaRegistro('Tipo Despacho', String(ids_Source_Frut.Object.defe_tipode[1]))
	of_InsertaRegistro('Forma Pago', '')
	of_InsertaRegistro('Codigo Vendedor', '')
	
	ls_Rut = String(Long(Mid(This.RutRecibidor, 1, Len(String(This.RutRecibidor)) - 1))) + '-' + Right(This.RutRecibidor, 1)
	
	of_InsertaRegistro('Rut Receptor', ls_Rut)
	of_InsertaRegistro('Codigo Interno Receptor', '')
	of_InsertaRegistro('Razon Social Receptor', This.Recibidor)
	of_InsertaRegistro('Giro Receptor', Mid(ids_Source_Frut.Object.embc_giroem[1], 1, 40)) 
	of_InsertaRegistro('Contacto Receptor', '')
	of_InsertaRegistro('Direccion Receptor', ids_Source_Frut.Object.embc_dirori[1])
	of_InsertaRegistro('Comuna Receptor', ids_Source_Frut.Object.embc_comuna[1])
	of_InsertaRegistro('Ciudad Receptor', ids_Source_Frut.Object.embc_ciudad[1])
	
	ls_Rut = String(Long(Mid(ids_Source_Frut.Object.defe_chfrut[1], 1, Len(String(ids_Source_Frut.Object.defe_chfrut[1])) - 1))) + '-' + Right(ids_Source_Frut.Object.defe_chfrut[1], 1)
	
	of_InsertaRegistro('RUT Chofer', ls_Rut)	
	of_InsertaRegistro('xCelular Chofer', ids_Source_Frut.Object.defe_celcho[1])
	of_InsertaRegistro('Nombre Chofer', ids_Source_Frut.Object.defe_chofer[1])
	of_InsertaRegistro("Patente Transporte", ids_Source_Frut.Object.defe_patent[1])
	of_InsertaRegistro("xPatenteCarro", ids_Source_Frut.Object.defe_pataco[1])
	
	of_InsertaRegistro('Direccion Origen', gstr_parempresa.empr_direcc) 
	of_InsertaRegistro('Comuna Origen', gstr_parempresa.empr_comuna)
	of_InsertaRegistro('Ciudad Origen', gstr_parempresa.empr_ciudad)

	of_InsertaRegistro('Direccion Destino', ids_Source_Frut.Object.embc_direcc[1])
	of_InsertaRegistro('Comuna Destino', ids_Source_Frut.Object.embc_comdes[1])
	of_InsertaRegistro('xSitiodestino', ids_Source_Frut.Object.defe_nomdes[1])
	
	of_InsertaRegistro('Monto Neto', '0')
	of_InsertaRegistro('Monto Exento', '')
	of_InsertaRegistro('Tasa Iva', '19')
	of_InsertaRegistro('Iva', '')
	of_InsertaRegistro('Monto Total', '0')
	of_InsertaRegistro('xMontoEscrito', '')
	of_InsertaRegistro('xObservaciones', Mid(ids_Source_Frut.Object.defe_glosa[1], 1, 120))
	of_InsertaRegistro('xObservaciones1', Mid("Fruta área libre de lobesia botrana - Fruta libre de área reglamentada MMe. " + ids_Source_Frut.Object.defe_glosas[1], 1, 120))
	of_InsertaRegistro('xObservaciones2', Mid('Fruta Certificada GLOBALG.A.P. GGN : ' + ids_Source_Frut.Object.ggn[1], 1, 120))
	of_InsertaRegistro('xUsuario Emision', gstr_us.Nombre)
	of_InsertaRegistro('xLugar Emision', iuo_Planta.Nombre)
	
	//Genera Detalle
	of_InsertaRegistro('DETALLE', ls_Referencia)	
	of_InsertaRegistro('', "Nro.Linea|Tipo codigo|Codigo del Item|Indicador Exencion|Nombre del Item|Descripcion Adicional Item|Cantidad|Unidad de Medida|Precio Unitario Item|Monto Item")
	For ll_Fila = 1 To ids_Source_Frut.RowCount()
		
		ls_Referencia = String(ll_Fila) + _SeparadorDTE
		ls_Referencia += '' + _SeparadorDTE
		ls_Referencia += '' + _SeparadorDTE
		ls_Referencia += '' + _SeparadorDTE
		ls_Referencia += Mid('Caj ' + Trim(ids_Source_Frut.Object.espe_nombre[ll_Fila]) + ' Exp.' + Trim(ids_Source_Frut.Object.emba_codigo[ll_Fila]) + ' ' + Trim(ids_Source_Frut.Object.vari_nombre[ll_Fila]) + ' Cal.' +Trim(ids_Source_Frut.Object.pafr_calibr[ll_Fila])  + ' - ' + String(ids_Source_Frut.Object.enva_pesone[ll_Fila], '#,##0.00') + ' Kn.' + String(ids_Source_Frut.Object.enva_pesobr[ll_Fila], '#,##0.00') + ' Kb.' /*+ ' CSG:' + Trim(ids_Source_Frut.Object.CSG[ll_Fila]) + ' CSP:' + Trim(ids_Source_Frut.Object.CSP[ll_Fila]) + ' SDP:' + Trim(ids_Source_Frut.Object.SDP[ll_Fila])*/,1,80)+ _SeparadorDTE
		ls_Referencia += 'Caj ' + Trim(ids_Source_Frut.Object.espe_nombre[ll_Fila]) + ' Exp.' + Trim(ids_Source_Frut.Object.emba_codigo[ll_Fila]) + ' ' + Trim(ids_Source_Frut.Object.vari_nombre[ll_Fila]) + ' Cal.' +Trim(ids_Source_Frut.Object.pafr_calibr[ll_Fila])  + ' - ' + String(ids_Source_Frut.Object.enva_pesone[ll_Fila], '#,##0.00') + ' Kn.' + String(ids_Source_Frut.Object.enva_pesobr[ll_Fila], '#,##0.00') + ' Kb.'+ ' CSG:' + Trim(ids_Source_Frut.Object.CSG[ll_Fila]) + ' CSP:' + Trim(ids_Source_Frut.Object.CSP[ll_Fila]) + ' SDP:' + Trim(ids_Source_Frut.Object.SDP[ll_Fila]) + _SeparadorDTE
		ls_Referencia += String(ids_Source_Frut.Object.paen_ccajas[ll_Fila], "#,##0") + _SeparadorDTE
		ls_Referencia += 'UNID' + _SeparadorDTE
		ls_Referencia += '' + _SeparadorDTE
		ls_Referencia += '0'
		
		of_InsertaRegistro('', ls_Referencia)
	Next

	SetNull(ls_Referencia)
	//Genera Referencia
	of_InsertaRegistro('REFERENCIA', ls_Referencia)
	of_InsertaRegistro('', "Nro Linea Referencia|Tipo Documento Referencia|Folio Referencia|Fecha Referencia|Codigo Referencia")
	
	//Graba Archivo
	of_GrabaArchivo(il_NroGuia, 1)
End If

Return lb_Retorno 
end function

private function string of_codigocsg (integer cliente, integer planta, long planilla);String	ls_Retorno , ls_Union = '-'
Long	ll_Fila

If ids_CSG.Retrieve(Planta, Cliente, Planilla) = 0 Then
	MessageBox("Error", "No se encontro informacion para la planilla.", Information!, OK!)
	ls_Retorno = ''
Else
	For ll_Fila = 1 To ids_CSG.RowCount()
		If ll_Fila = ids_CSG.RowCount() Then
			ls_Retorno += ids_CSG.Object.prpr_prepro[ll_Fila]
		Else
			ls_Retorno += ids_CSG.Object.prpr_prepro[ll_Fila] + ls_Union
		End If		
	Next	
End If

Return ls_Retorno
end function

private function string of_codigocsp (integer cliente, integer planta, long planilla);String	ls_Retorno , ls_Union = '-'
Long	ll_Fila

If ids_CSP.Retrieve(Planta, Cliente, Planilla) = 0 Then
	MessageBox("Error", "No se encontro informacion para la planilla.", Information!, OK!)
	ls_Retorno = ''
Else
	For ll_Fila = 1 To ids_CSP.RowCount()
		If ll_Fila = ids_CSP.RowCount() Then
			ls_Retorno += ids_CSP.Object.plde_chasag[ll_Fila]
		Else
			ls_Retorno += ids_CSP.Object.plde_chasag[ll_Fila] + ls_Union
		End If		
	Next	
End If

Return ls_Retorno
end function

private subroutine of_settipotransporte (integer codigo);If IsNull(Codigo) Then Codigo = 0
This.TipoTransporte	= Codigo
end subroutine

public function long of_emiteguia (long planta, long cliente, long movimiento, boolean conecta, long variedad, integer productor, integer calibre, integer consvariedad, integer consembalaje, integer conscalibre, integer resumen, integer tipo, long comprobante);Integer li_Conexion

If Tipo = 1 Then 
	li_Conexion = gstr_ParEmpresa.Conecion_GuiaElectronica
Else
	li_Conexion = gstr_ParEmpresa.Conecion_GuiaElectronica_Comer
End If

If Conecta Then
	If of_Conectar(li_Conexion) Then
		il_NroGuia = of_ObtieneNroGuia(_TipoDocto, _Emisor, SQLProd)
		If il_NroGuia = -1 Then
			MessageBox('Error SQLProd', 'No hay correlativos disponibles, se utilizo el ultimo.', StopSign!, OK!)
		ElseIf il_NroGuia = -2 Then
			MessageBox('Error SQLProd', 'No existen rangos vigentes para correlativos.', StopSign!, OK!)
		Else
			If of_DesConectar() Then 
				of_GeneraGuia(Planta, Cliente, Movimiento, Variedad, Productor, Calibre, ConsVariedad, ConsEmbalaje, ConsCalibre, Resumen, Tipo, Long(Comprobante))
			Else
				MessageBox('Error SQLProd', 'No pudo desconectare de la base emisora de Folios.', StopSign!, OK!)
			End If
		End If
	Else
//		MessageBox('Error', 'No se pudo conectar con la base emisora de Folios.', StopSign!, OK!)
//		il_NroGuia = -3
		If of_Conectar(li_Conexion) Then
			il_NroGuia = of_ObtieneNroGuia(_TipoDocto, _Emisor, SQLCA)
			If il_NroGuia = -1 Then
				MessageBox('Error SQLCA', 'No hay correlativos disponibles, se utilizo el ultimo.', StopSign!, OK!)
			ElseIf il_NroGuia = -2 Then
				MessageBox('Error SQLCA', 'No existen rangos vigentes para correlativos.', StopSign!, OK!)
			Else
				of_GeneraGuia(Planta, Cliente, Movimiento, Variedad, Productor, Calibre, ConsVariedad, ConsEmbalaje, ConsCalibre, Resumen, Tipo, Long(Comprobante))
			End If
		Else
			MessageBox('Error SQLCA', 'No se pudo conectar con la base emisora de Folios.', StopSign!, OK!)
			il_NroGuia = -3
		End If
	End If
Else
	il_NroGuia	= Movimiento
	If Not of_GeneraGuia(Cliente, Planta, Movimiento, Variedad, Productor, Calibre, ConsVariedad,&
			ConsEmbalaje, ConsCalibre, Resumen, Tipo, Comprobante) Then il_NroGuia = -3
End If 

Return il_NroGuia
end function

private function boolean of_generaguia (long planta, long cliente, long movimiento, long variedad, integer productor, integer calibre, integer consvariedad, integer consembalaje, integer conscalibre, integer resumen, integer tipo, long comprobante);Boolean	lb_Retorno = True
Long		ll_New, ll_Fila, ll_Linea
String		ls_Referencia, ls_Rut, ls_PuertoOrigen, ls_Puerto, ls_Pais, ls_Totales

SetNull(ls_Referencia)

IVA = of_IvaPlanta(Planta)

If IVA = -1 Then
	Messagebox('Error', 'No esta configurado el porcentaje de IVA en Parametros de Planta, se utilizara 19%.', StopSign!, OK!)
	IVA = 19
End If

IF of_DatosEmpresa(Tipo) Then
	//Movimiento
	If Tipo = 1 Or Tipo = 0 Then
		ll_Fila = ids_Source.Retrieve(Cliente, Planta, Movimiento, 1, Variedad, Productor, Calibre, ConsVariedad, ConsEmbalaje, ConsCalibre, Resumen, Comprobante)
	Else
		ll_Fila = ids_Source.Retrieve(Cliente, Planta, Variedad, Productor, Calibre)
	End If
	
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
		of_SetTipoTransporte(ids_Source.Object.defe_tipotr[1])
		
		ls_Rut = String(Long(Mid(This.RutRecibidor, 1, Len(String(This.RutRecibidor)) - 1))) + '-' + Right(This.RutRecibidor, 1)
	
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
		of_InsertaRegistro('Razon Social Receptor', This.Recibidor)
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
		
		of_InsertaRegistro('Direccion Origen', This.Direccion) 
		of_InsertaRegistro('Comuna Origen', This.Comuna)
		of_InsertaRegistro('Ciudad Origen', This.Ciudad)
	
		of_InsertaRegistro('Direccion Destino', ids_Source.Object.embc_direcc[1])
		of_InsertaRegistro('Comuna Destino', ids_Source.Object.embc_comdes[1])
		of_InsertaRegistro('xSitiodestino', ids_Source.Object.defe_nomdes[1])
		
		If Tipo = 1 Or Tipo = 0 Then 
			of_InsertaRegistro('Monto Neto', '0')
			of_InsertaRegistro('Iva', '')
			of_InsertaRegistro('Monto Total', '0')
		Else
			of_InsertaRegistro('Monto Neto', String(ids_Source.Object.Total_Neto[1], '#,##0'))
			If ids_Source.Object.Total_Iva[1] = 0 Then
				of_InsertaRegistro('Iva', '')
			Else
				of_InsertaRegistro('Iva', String(ids_Source.Object.Total_Iva[1], '#,##0'))
			End If
			
			of_InsertaRegistro('Monto Total', String(ids_Source.Object.Total[1], '#,##0'))
		End If
		
		of_InsertaRegistro('Monto Exento', '')
		of_InsertaRegistro('Tasa Iva', String(IVA))
		of_InsertaRegistro('xMontoEscrito', '')			
	
		If IsNull(ids_Source.Object.defe_glosas[1]) Then
			ls_Referencia = ''
		Else
			ls_Referencia = Trim(ids_Source.Object.defe_glosas[1])
			If Tipo = 1 Then
				ls_Referencia =  ls_Referencia + ' - Fruta Certificada GLOBALG.A.P. ' + f_Kilos_GGN(Cliente, Planta, Movimiento)
			End If
		End If
		
		of_InsertaRegistro('xObservaciones', Mid(ls_Referencia, 1, 120))
		
		If Tipo = 3 Then 
			If IsNull(ids_Source.Object.glosas[1]) Then
				ls_Referencia = ''
			Else
				ls_Referencia = Trim(ids_Source.Object.glosas[1])
			End If
			
			of_InsertaRegistro('xObservaciones1', Mid(ls_Referencia, 1, 120))
			of_InsertaRegistro('xObservaciones2', Mid('Fruta Certificada GLOBALG.A.P. GGN : ' + ids_Source.Object.ggn[1], 1, 120))
		ElseIf Tipo = 4  Then
			of_InsertaRegistro('xObservaciones2', Mid('Fruta Certificada GLOBALG.A.P. GGN : ' + ids_Source.Object.ggn[1], 1, 120))
		ElseIf Tipo = 1 Then //Guia Exportacion / Embarques
			//Glosa 1
			ls_Referencia = Mid('Total VGM: ' + String(ids_Source.Object.defe_totvgm[1], '#,##0.00') + &
								', Sello(s) N : ' + CantidadSellos + ' - ' + NumerosSellos + &
								', Ubicación : ' + UbicacionSello +  ', Multiuso : ' + Trim(ids_Source.Object.defe_term01[1]) + '/' + &
								Trim(ids_Source.Object.defe_term02[1]) + '/' + Trim(ids_Source.Object.defe_term03[1]),1,120)		
			of_InsertaRegistro('xObservaciones1', ls_Referencia)
			//Glosa 2
			ls_Referencia = Mid('Sello Naviera: ' + Trim(ids_Source.Object.defe_selnav[1]) + &
								', Reserva: ' + Trim(ids_Source.Object.embq_bookin[1]) + &
								', Planilla SAG: ' + String(ids_Source.Object.defe_nrosps[1]) + ', Termografo : '  + &
								of_GeneraTermografos(Cliente, Planta, Movimiento), 1, 120)
								
			of_InsertaRegistro('xObservaciones2', ls_Referencia)
			
			//of_InsertaRegistro('xObservaciones2', Mid('Fruta Certificada Global GAP, ' + f_Kilos_GGN(Cliente, Planta, Movimiento), 1, 120))
			
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
		End If
		
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
			
			If Tipo = 1 Then 
				ls_Referencia += Mid('Cajas ' + Trim(ids_Source.Object.espe_nombre[ll_Fila]) + ' Exp. ' + Trim(ids_Source.Object.emba_codigo[ll_Fila]) + ' ' + Trim(ids_Source.Object.vari_nombre[ll_Fila]) + ' Cal.' +Trim(ids_Source.Object.pafr_calibr[ll_Fila])  + ' - ' + String(ids_Source.Object.enva_pesone[ll_Fila], '#,##0.00') + ' Kn. ' + String(ids_Source.Object.enva_pesobr[ll_Fila], '#,##0.00') + ' Kb.' ,1,80)+ _SeparadorDTE
				ls_Referencia += 'Cajas ' + Trim(ids_Source.Object.espe_nombre[ll_Fila]) + ' Exp. ' + Trim(ids_Source.Object.emba_codigo[ll_Fila]) + ' ' + Trim(ids_Source.Object.vari_nombre[ll_Fila]) + ' Cal.' +Trim(ids_Source.Object.pafr_calibr[ll_Fila])  + ' - ' + String(ids_Source.Object.enva_pesone[ll_Fila], '#,##0.00') + ' Kn. ' + String(ids_Source.Object.enva_pesobr[ll_Fila], '#,##0.00') + ' Kb.' + _SeparadorDTE
			ElseIf Tipo = 0 Then
				ls_Referencia += Mid('Cajas muestra ' + Trim(ids_Source.Object.espe_nombre[ll_Fila]) + ' Lote ' + Trim(String(ids_Source.Object.defi_nrlote[ll_Fila], '000000')) + ' - ' + String(ids_Source.Object.enva_pesone[ll_Fila], '#,##0.00') + ' Kn. ',1,80)+ _SeparadorDTE
				ls_Referencia +=  'Cajas muestra ' + Trim(ids_Source.Object.espe_nombre[ll_Fila]) + ' Lote ' + Trim(String(ids_Source.Object.defi_nrlote[ll_Fila], '000000')) + ' - ' + String(ids_Source.Object.enva_pesone[ll_Fila], '#,##0.00') + ' Kn. '+ _SeparadorDTE
			Else
				ls_Referencia += Mid(Trim(ids_Source.Object.Detalle[ll_Fila]) ,1,80)+ _SeparadorDTE
				ls_Referencia += Trim(ids_Source.Object.Detalle[ll_Fila]) + _SeparadorDTE
			End If
			
			If Tipo = 1 Or Tipo = 0 Then 
				ls_Referencia += String(ids_Source.Object.paen_ccajas[ll_Fila], "#,##0") + _SeparadorDTE
				ls_Referencia += 'UNID' + _SeparadorDTE
			Else
				If ids_Source.Object.enva_codigo[ll_Fila] = 0 Then 
					ls_Referencia += String(ids_Source.Object.paen_ccajas[ll_Fila], "#,##0.00") + _SeparadorDTE
					ls_Referencia += 'KILO' + _SeparadorDTE
				Else
					ls_Referencia += String(ids_Source.Object.paen_ccajas[ll_Fila], "#,##0") + _SeparadorDTE
					ls_Referencia += 'UNID' + _SeparadorDTE
				End If
			End If
			
			If Tipo = 1 Or Tipo = 0 Then 
				ls_Referencia += '' + _SeparadorDTE
				ls_Referencia += '0'
			Else
				If ids_Source.Object.mfcd_preuni[ll_Fila] = 0 Then
					ls_Referencia += '' + _SeparadorDTE
				Else
					ls_Referencia += String(ids_Source.Object.mfcd_preuni[ll_Fila], "#,##0") + _SeparadorDTE
				End If
				ls_Referencia += String(ids_Source.Object.Neto[ll_Fila], "#,##0")
			End If
				
			
			of_InsertaRegistro('', ls_Referencia)
		Next
		
		If Tipo = 1 Or Tipo = 0 Then //Detalle Guia Embarques (1) y Lotes USDA (0)
			//Genera Item con totales
			ll_Linea = ll_Fila 
			
			If Tipo = 1 Then
				ls_Totales = 'Total Cajas : ' + String(ids_Source.Object.Total_Cajas[1], '#,##0') + &
								' / Total KN: ' + String(ids_Source.Object.Total_KNetos[1], '#,##0.00') + &
								' / Total KB: ' + String(ids_Source.Object.Total_KBrutos[1], '#,##0.00')
			ElseIf Tipo = 0 Then
				ls_Totales = 'Total Cajas : ' + String(ids_Source.Object.Total_Cajas[1], '#,##0') + &
								' / Total KN: ' + String(ids_Source.Object.Total_KNetos[1], '#,##0.00') 
			End If
	
			ls_Referencia = String(ll_Linea) + _SeparadorDTE
			ls_Referencia += '' + _SeparadorDTE
			ls_Referencia += '' + _SeparadorDTE
			ls_Referencia += '2' + _SeparadorDTE
			ls_Referencia += ls_Totales + _SeparadorDTE
			ls_Referencia += ls_Totales + _SeparadorDTE
			ls_Referencia += ''+ _SeparadorDTE
			ls_Referencia += '' + _SeparadorDTE
			ls_Referencia += '' + _SeparadorDTE
			ls_Referencia += '0'
			
			of_InsertaRegistro('', ls_Referencia)
			ll_Linea++
				
			If Tipo = 1 Then
				//Genera Detalle IFCO
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
					
					ls_Referencia = String(ll_Linea) + _SeparadorDTE
					ls_Referencia += '' + _SeparadorDTE
					ls_Referencia += '' + _SeparadorDTE
					ls_Referencia += '2' + _SeparadorDTE
					ls_Referencia += String(ids_Cajas.Object.TotalPallet[1], '#,##0') + ' Total de Pallets' + _SeparadorDTE
					ls_Referencia += String(ids_Cajas.Object.TotalPallet[1], '#,##0') + ' Total de Pallets' + _SeparadorDTE
					ls_Referencia += ''+ _SeparadorDTE
					ls_Referencia += '' + _SeparadorDTE
					ls_Referencia += '' + _SeparadorDTE
					ls_Referencia += '0'
					
					of_InsertaRegistro('', ls_Referencia)	
				End If
				//Detalle de codigos CSG
				ls_Referencia = String(ll_Linea) + _SeparadorDTE
				ls_Referencia += '' + _SeparadorDTE
				ls_Referencia += '' + _SeparadorDTE
				ls_Referencia += '2' + _SeparadorDTE
				ls_Referencia += Mid(' CSG / ' + of_CodigoCSG(Cliente, Planta, Movimiento), 1, 80) + _SeparadorDTE
				ls_Referencia += ' CSG / ' + of_CodigoCSG(Cliente, Planta, Movimiento) + _SeparadorDTE
				ls_Referencia += ''+ _SeparadorDTE
				ls_Referencia += '' + _SeparadorDTE
				ls_Referencia += '' + _SeparadorDTE
				ls_Referencia += '0'
				
				of_InsertaRegistro('', ls_Referencia)	
				
				//Detalle de codigos CSP
				ls_Referencia = String(ll_Linea) + _SeparadorDTE
				ls_Referencia += '' + _SeparadorDTE
				ls_Referencia += '' + _SeparadorDTE
				ls_Referencia += '2' + _SeparadorDTE
				ls_Referencia += Mid(' CSP / ' + of_CodigoCSP(Cliente, Planta, Movimiento), 1, 80) + _SeparadorDTE
				ls_Referencia += ' CSP / ' + of_CodigoCSP(Cliente, Planta, Movimiento) + _SeparadorDTE
				ls_Referencia += ''+ _SeparadorDTE
				ls_Referencia += '' + _SeparadorDTE
				ls_Referencia += '' + _SeparadorDTE
				ls_Referencia += '0'
				
				of_InsertaRegistro('', ls_Referencia)
					
			End If
		End If
		
		SetNull(ls_Referencia)
		//Genera Referencia
		of_InsertaRegistro('REFERENCIA', ls_Referencia)
		of_InsertaRegistro('', "Nro Linea Referencia|Tipo Documento Referencia|Folio Referencia|Fecha Referencia|Codigo Referencia")
		
		//Graba Archivo
		of_GrabaArchivo(il_NroGuia, Tipo)
	End If
Else
	lb_Retorno = False
End If

Return lb_Retorno 
end function

private function boolean of_generaguia_fruticolagde (long planta, long cliente, long movimiento, integer tipo);Boolean	lb_Retorno = True
Long		ll_New, ll_Fila
String		ls_Referencia, ls_Rut, ls_Mosca = ''

SetNull(ls_Referencia)
//Movimiento
If ids_Source_GDE.Retrieve(Cliente, Planta, Movimiento,  Tipo, il_NroGuia) = -1 Then
	MessageBox('Error', 'No se pudo recuperar informacion de Guia', StopSign!, OK!)
	lb_Retorno = False
Else
	//Genera Archivo DTE
	ids_Guia.Reset()
	
	of_SetRutRecibidor(ids_Source_GDE.Object.embc_nrorut[1])
	of_SetRecibidor(ids_Source_GDE.Object.embc_nombre[1])
	of_SetTipoTransporte(ids_Source_GDE.Object.defe_tipotr[1])
	
	//Genera Encabezado
	of_InsertaRegistro('ENCABEZADO', ls_Referencia)
	
	of_InsertaRegistro('Tipo DTE', String(_TipoDoctoDTE))
	of_InsertaRegistro('Folio', String(il_NroGuia))
	of_InsertaRegistro('Fecha de Emision', String(ids_Source_GDE.Object.defe_fecdes[1], 'dd-mm-yyyy'))
	of_InsertaRegistro('Ind. traslado de bienes', String(ids_Source_GDE.Object.defe_tipotr[1]))
	of_InsertaRegistro('Tipo Despacho', String(ids_Source_GDE.Object.defe_tipode[1]))
	of_InsertaRegistro('Forma Pago', '')
	of_InsertaRegistro('Codigo Vendedor', '')
	
	ls_Rut = String(Long(Mid(This.RutRecibidor, 1, Len(String(This.RutRecibidor)) - 1))) + '-' + Right(This.RutRecibidor, 1)
	
	of_InsertaRegistro('Rut Receptor', ls_Rut)
	of_InsertaRegistro('Codigo Interno Receptor', '')
	of_InsertaRegistro('Razon Social Receptor', This.Recibidor)
	of_InsertaRegistro('Giro Receptor', Mid(ids_Source_GDE.Object.embc_giroem[1], 1, 40)) 
	of_InsertaRegistro('Contacto Receptor', '')
	of_InsertaRegistro('Direccion Receptor', ids_Source_GDE.Object.embc_dirori[1])
	of_InsertaRegistro('Comuna Receptor', ids_Source_GDE.Object.embc_comuna[1])
	of_InsertaRegistro('Ciudad Receptor', ids_Source_GDE.Object.embc_ciudad[1])
	
	ls_Rut = String(Long(Mid(ids_Source_GDE.Object.defe_chfrut[1], 1, Len(String(ids_Source_GDE.Object.defe_chfrut[1])) - 1))) + '-' + Right(ids_Source_GDE.Object.defe_chfrut[1], 1)
	
	of_InsertaRegistro('RUT Chofer', ls_Rut)	
	of_InsertaRegistro('xCelular Chofer', ids_Source_GDE.Object.defe_celcho[1])
	of_InsertaRegistro('Nombre Chofer', ids_Source_GDE.Object.defe_chofer[1])
	of_InsertaRegistro("Patente Transporte", ids_Source_GDE.Object.defe_patent[1])
	of_InsertaRegistro("xPatenteCarro", ids_Source_GDE.Object.defe_pataco[1])
	
	of_InsertaRegistro('Direccion Origen', gstr_parempresa.empr_direcc) 
	of_InsertaRegistro('Comuna Origen', gstr_parempresa.empr_comuna)
	of_InsertaRegistro('Ciudad Origen', gstr_parempresa.empr_ciudad)

	of_InsertaRegistro('Direccion Destino', ids_Source_GDE.Object.embc_direcc[1])
	of_InsertaRegistro('Comuna Destino', ids_Source_GDE.Object.embc_comdes[1])
	of_InsertaRegistro('xSitiodestino', ids_Source_GDE.Object.defe_nomdes[1])
	
	If Tipo = 2 Then 
		of_InsertaRegistro('Monto Neto', '0')
		of_InsertaRegistro('Iva', '')
		of_InsertaRegistro('Monto Total', '0')
	Else
		of_InsertaRegistro('Monto Neto', String(ids_Source_GDE.Object.Total_Neto[1], '#,##0'))
		If ids_Source_GDE.Object.Total_Iva[1] = 0 Then
			of_InsertaRegistro('Iva', '')
		Else
			of_InsertaRegistro('Iva', String(ids_Source_GDE.Object.Total_Iva[1], '#,##0'))
		End If
		
		of_InsertaRegistro('Monto Total', String(ids_Source_GDE.Object.Total[1], '#,##0'))
	End If	
	
	If Planta <> 4014 Then ls_Mosca = ' \ FRUTA PROVENIENTE DE AREA LIBRE DE MOSCA DE LA FRUTA'
	
	of_InsertaRegistro('Monto Exento', '')
	of_InsertaRegistro('Tasa Iva', '19')
	of_InsertaRegistro('xMontoEscrito', '')
	of_InsertaRegistro('xObservaciones', Mid(ids_Source_GDE.Object.defe_glosa[1], 1, 120))
	of_InsertaRegistro('xObservaciones1', Mid(ids_Source_GDE.Object.defe_glosas[1] + ls_Mosca, 1, 120))
	of_InsertaRegistro('xObservaciones2', Mid('Productor : ' + String(ids_Source_GDE.Object.prod_codigo[1], '0000') + ' \ FRUTA CERTIFICADA GLOBALG.A.P. GGN : ' + ids_Source_GDE.Object.ggn[1], 1, 120))
	of_InsertaRegistro('xObservaciones2', Mid(' CSG:' + ids_Source_GDE.Object.CSG[1] + ' CSP:' + ids_Source_GDE.Object.CSP[1], 1, 120))
	of_InsertaRegistro('xUsuario Emision', gstr_us.Nombre)
	of_InsertaRegistro('xLugar Emision', iuo_Planta.Nombre)
	
	//Genera Detalle
	of_InsertaRegistro('DETALLE', ls_Referencia)	
	of_InsertaRegistro('', "Nro.Linea|Tipo codigo|Codigo del Item|Indicador Exencion|Nombre del Item|Descripcion Adicional Item|Cantidad|Unidad de Medida|Precio Unitario Item|Monto Item")
	For ll_Fila = 1 To ids_Source_GDE.RowCount()
		
		ls_Referencia = String(ll_Fila) + _SeparadorDTE
		ls_Referencia += '' + _SeparadorDTE
		ls_Referencia += '' + _SeparadorDTE
		ls_Referencia += '' + _SeparadorDTE
		ls_Referencia += Mid(Trim(ids_Source_GDE.Object.espe_nombre[ll_Fila]) + ' ' + Trim(ids_Source_GDE.Object.vari_nombre[ll_Fila]) + ' ' + Trim(ids_Source_GDE.Object.cate_nombre[ll_Fila]) + ' Cal.' +Trim(ids_Source_GDE.Object.pafr_calibr[ll_Fila])  + ' - ' + ' SDP: '  + ids_Source_GDE.Object.SDP[ll_Fila] + ' COLOR: ' +  ids_Source_GDE.Object.Color[ll_Fila]  + ' - ' + ids_Source_GDE.Object.Cantidad[ll_Fila],1,80) + _SeparadorDTE
		ls_Referencia += Trim(ids_Source_GDE.Object.espe_nombre[ll_Fila]) + ' ' + Trim(ids_Source_GDE.Object.vari_nombre[ll_Fila]) + ' ' + Trim(ids_Source_GDE.Object.cate_nombre[ll_Fila]) + ' Cal.' +Trim(ids_Source_GDE.Object.pafr_calibr[ll_Fila])  + ' - ' + ' SDP: '  + ids_Source_GDE.Object.SDP[ll_Fila] + ' COLOR: ' +  ids_Source_GDE.Object.Color[ll_Fila] + ' - ' + ids_Source_GDE.Object.Cantidad[ll_Fila] + _SeparadorDTE		
		ls_Referencia += String(ids_Source_GDE.Object.paen_ccajas[ll_Fila], "#,##0") + _SeparadorDTE
		ls_Referencia += 'KILO' + _SeparadorDTE
//		ls_Referencia += '' + _SeparadorDTE
//		ls_Referencia += '0'
		
		If Tipo = 2 Then 
			ls_Referencia += '' + _SeparadorDTE
			ls_Referencia += '0'
		Else
			If ids_Source_GDE.Object.emvf_valfob[ll_Fila] = 0 Then
				ls_Referencia += '' + _SeparadorDTE
			Else
				ls_Referencia += String(ids_Source_GDE.Object.emvf_valfob[ll_Fila], "#,##0") + _SeparadorDTE
			End If
			ls_Referencia += String(ids_Source_GDE.Object.paen_ccajas[ll_Fila] * ids_Source_GDE.Object.emvf_valfob[ll_Fila], "#,##0")
		End If

		of_InsertaRegistro('', ls_Referencia)
	Next

	SetNull(ls_Referencia)
	//Genera Referencia
	of_InsertaRegistro('REFERENCIA', ls_Referencia)
	of_InsertaRegistro('', "Nro Linea Referencia|Tipo Documento Referencia|Folio Referencia|Fecha Referencia|Codigo Referencia")
	
	//Graba Archivo
	of_GrabaArchivo(il_NroGuia, 1)
End If

Return lb_Retorno 
end function

public function long of_emiteguia_fruticolagde (long planta, long cliente, integer movimiento, integer tipo);
If of_Conectar(gstr_ParEmpresa.Conecion_GuiaElectronica) Then
	il_NroGuia = of_ObtieneNroGuia(_TipoDocto, _Emisor, SQLProd)
	If il_NroGuia = -1 Then
		MessageBox('Error', 'No se correlativos disponibles, se utilizo el ultimo.', StopSign!, OK!)
	ElseIf il_NroGuia = -2 Then
		MessageBox('Error', 'No se existen rangos vigentes para correlativos.', StopSign!, OK!)
	Else
		If of_DesConectar() Then 
			of_GeneraGuia_FruticolaGDE(Planta, Cliente, Movimiento, Tipo)
		Else
			MessageBox('Error', 'No se pudo desconectar de la base de Folios.', StopSign!, OK!)
		End If
	End If
Else
	il_NroGuia = of_ObtieneNroGuia(_TipoDocto, _Emisor, SQLCA)
	If il_NroGuia = -1 Then
		MessageBox('Error', 'No se correlativos disponibles, se utilizo el ultimo.', StopSign!, OK!)
	ElseIf il_NroGuia = -2 Then
		MessageBox('Error', 'No se existen rangos vigentes para correlativos.', StopSign!, OK!)
	Else
		of_GeneraGuia_FruticolaGDE(Planta, Cliente, Movimiento, Tipo)
	End If
	
//	MessageBox('Error', 'No se pudo conectar con la base emisora de Folios.', StopSign!, OK!)
End If

Return il_NroGuia
end function

public function boolean of_generalibroguia_gde (integer tipo);Boolean	lb_Retorno = True
Integer 	li_Conexion
Long		ll_Neto = 0, ll_Iva = 0, ll_Total = 0

li_Conexion = gstr_ParEmpresa.Conecion_GuiaElectronica

If of_Conectar(li_Conexion) Then
	
	If Tipo = 2 Then 
		INSERT INTO dba.contguiasdespacho (tpdo_codigo, guia_numero, guia_estado, guia_anumod, guia_tipope, guia_fechag, clpr_rut, guia_nomrec, 
													 guia_valnet, guia_poriva, guia_valiva, guia_valtot, guia_valmod, guia_tdrefe, guia_docref, guia_fecref, 
													 ceem_codigo, guia_usuemi, guia_modori, guia_feccre, guia_usumod, guia_fecmod, bode_codigo) 
			  VALUES ( 28, :il_NroGuia, 1, Null, :This.TipoTransporte, GetDate(), :This.RutRecibidor, SubString(:This.Recibidor,1,60), 
						0, 0, 0, 0, 0, Null, Null, Null, 
						1, :gstr_us.Nombre, SubString(:gstr_apl.NombreSistema,1,40), GetDate(), Null, Null, Null)
			Using SQLProd;
	Else
		ll_Neto	=	ids_Source_GDE.Object.Total_Neto[1]
		ll_Iva		=	ids_Source_GDE.Object.Total_Iva[1]
		ll_Total	=	ids_Source_GDE.Object.Total[1]
		
		INSERT INTO dba.contguiasdespacho (tpdo_codigo, guia_numero, guia_estado, guia_anumod, guia_tipope, guia_fechag, clpr_rut, guia_nomrec, 
											 guia_valnet, guia_poriva, guia_valiva, guia_valtot, guia_valmod, guia_tdrefe, guia_docref, guia_fecref, 
											 ceem_codigo, guia_usuemi, guia_modori, guia_feccre, guia_usumod, guia_fecmod, bode_codigo) 
			VALUES ( 28, :il_NroGuia, 1, Null, :This.TipoTransporte, GetDate(), :This.RutRecibidor, SubString(:This.Recibidor,1,60), 
						:ll_Neto, :IVA, :ll_Iva, :ll_Total,
						0, Null, Null, Null, 
						1, :gstr_us.Nombre, SubString(:gstr_apl.NombreSistema,1,40), GetDate(), Null, Null, Null)
			Using SQLProd;
	End If
	
	If SQLProd.SQLCode = -1 Then
		F_ErrorBaseDatos(SQLProd, "Lectura Carga de Libro de Guia Electronicas")
		lb_Retorno =  False
	Else
		If of_DesConectar() Then 
		End If
	End If
Else
	MessageBox('Error', 'No se pudo conectar con la base para generar libro de Guias.', StopSign!, OK!)
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

event constructor;SQLProd				=	Create Transaction
ids_Guia				=	Create DataStore
ids_Source			=	Create DataStore
ids_Source_Frut	=	Create DataStore
ids_Source_GDE	=	Create DataStore
ids_Termografo	=	Create DataStore
ids_IFCO				=	Create DataStore
ids_Pallet			=	Create DataStore
ids_Cajas			=	Create DataStore
ids_CSG				=	Create DataStore
ids_CSP				=	Create DataStore
iuo_Planta			=	Create uo_Plantadesp
iuo_Coneccion		=	Create uo_Conectividad
iuo_BuscaArchivo	=	Create n_buscaarchivo	

ids_CSG.DataObject = 'dw_gene_codigoscsg_guia'
ids_CSG.SetTransObject(Sqlca)

ids_CSP.DataObject = 'dw_gene_codigoscsp_guia'
ids_CSP.SetTransObject(Sqlca)

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

ids_Source_Frut.DataObject = 'dw_info_guia_despacho_frut'
ids_Source_Frut.SetTransObject(Sqlca)

ids_Source_GDE.DataObject = 'dw_info_guia_despacho_gde'
ids_Source_GDE.SetTransObject(Sqlca)

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
Destroy ids_Source_Frut
Destroy ids_Source_GDE
Destroy ids_Pallet
Destroy ids_Cajas
Destroy ids_Termografo
Destroy ids_IFCO
Destroy ids_Pallet
Destroy ids_CSG
Destroy ids_CSP
Destroy iuo_Planta
Destroy iuo_BuscaArchivo
end event

